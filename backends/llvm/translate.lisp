(in-package #:hare-llvm)

(defvar *module*)

(defmacro with-module ((&rest args) &body body)
  `(llvm:with-object (*module* llvm:module ,@args)
     (setf (llvm:target *module*)
           "x86_64-pc-linux-gnu")
     ,@body))

;;;

(defclass env ()
  (;; An alist of (variable . llvm-value)
   (%bindings :initarg :bindings :reader bindings :type list)))

(defun augment (env pairs)
  (make-instance 'env
    :bindings (append pairs (bindings env))))

(defun %lookup (variable env) (cdr (assoc variable (bindings env))))

(defun lookup (variable env)
  (or (%lookup variable env)
      (error "BUG in translate: Unknown variable ~a" variable)))

;;;

(defgeneric declare-variable (type &key name initializer))

(defmethod declare-variable ((type type:fun) &key (name "") initializer)
  (declare (ignore initializer))
  (let ((type (translate-type type)))
    (llvm:add-function *module* name type)))

(defmethod declare-variable ((type type:arrayt) &key (name "") initializer)
  (let* ((etype (translate-type (type:arrayt-element-type type)))
         (len (if initializer (length (ast:elements initializer)) 0))
         (etype* (llvm:array-type etype len))
         (glob (llvm:add-global *module* etype* name)))
    (setf (llvm:alignment glob) 8)
    glob))

;;;

(defun translate (manifest)
  (let* ((binds
           (append
            (loop for ext in (hare::externs manifest)
                  for name = (hare:name ext)
                  for var = (hare:variable ext)
                  for type = (hare::type ext)
                  for val = (declare-variable type :name name)
                  collect (cons var val))
            ;; TODO: externs here
            (loop for mani in (hare::monodefs manifest)
                  for name = (hare:name mani)
                  for var = (hare:variable mani)
                  for initializer = (hare:initializer mani)
                  for type = (ast:type initializer)
                  for val = (declare-variable type :name name
                                                   :initializer initializer)
                  collect (cons var val))))
         (env (make-instance 'env :bindings binds)))
    (loop for mani in (hare::monodefs manifest)
          for var = (hare:variable mani)
          for initializer = (hare:initializer mani)
          for val = (or (cdr (assoc var binds)) (error "HOW??"))
          do (translate-initializer initializer val env))))

(defun translate-type (type) (type->llvm type))

;;;

(defvar *builder*)

(defmacro with-builder ((&rest args) &body body)
  `(llvm:with-object (*builder* llvm:builder ,@args) ,@body))

;;;

(defgeneric translate-initializer (initializer valref global-env))

(defvar *function*)

(defmethod translate-initializer ((lambda-initializer ast:lambda-initializer)
                                  *function* global-env)
  (let* ((params (ast:params lambda-initializer))
         (body (ast:body lambda-initializer))
         (lparams (llvm:params *function*))
         (env (augment global-env (mapcar #'cons params lparams)))
         (entry (llvm:append-basic-block *function* "entry")))
    (with-builder ()
      (llvm:position-builder-at-end *builder* entry)
      (let ((r (translate-ast body env)))
        (unless (null r)
          (llvm:build-ret *builder* r)))
      *function*)))

(defmethod translate-initializer ((init ast:array-initializer)
                                  var global-env)
  (let* ((etype (type->llvm (type:arrayt-element-type (ast:type init))))
         (const (llvm:const-array etype
                                  ;; KLUDGE?
                                  (mapcar #'translate-literal
                                          (ast:elements init)))))
    (llvm:set-initializer var const)
    var))

;;;

(defgeneric translate-ast (ast env)
  (:documentation "Generate LLVM-IR for the given AST.
Return an LLVMValueRef for the result, or NIL if there isn't one
(e.g. if there is an escape)."))

(defmethod translate-ast ((ast ast:seq) env)
  (loop with result = nil
        for ast in (ast:asts ast)
        when (null (translate-ast ast env))
          do (return-from translate-ast nil))
  (translate-ast (ast:value ast) env))

(defmethod translate-ast ((ast ast:call) env)
  (let* ((callee (translate-ast (ast:callee ast) env))
         (_ (when (null callee) (return-from translate-ast nil)))
         (args (loop for arg in (ast:args ast)
                     for value = (translate-ast arg env)
                     when (null value)
                       do (return-from translate-ast nil)
                     else collect value)))
    (declare (ignore _))
    ;; name required or it will be void
    ;; ...but actually, we need to handle void returns for Unit, maybe?
    ;; FIXME
    (llvm:build-call *builder* callee args "")))

(defgeneric construct (layout adt constructor &rest args))
(defmethod construct ((layout direct-layout) adt constructor &rest args)
  (declare (ignore adt constructor))
  (let ((str (llvm:undef (ltype layout))))
    (loop for arg in args
          for i from 0
          do (setf str (llvm:build-insert-value *builder* str arg i "")))
    str))
(defgeneric wordify (type value))
(defmethod wordify ((type type:int) value)
  (list (if (< (type:int-type-length type) 64)
            (llvm:build-z-ext *builder* value (llvm:int64-type) "")
            value)))
(defmethod wordify ((type type:pointer) value)
  (list (llvm:build-pointer-to-int *builder* value (llvm:int64-type) "")))
(defmethod wordify ((type type:adt) value)
  (if (direct-layout-adt-p type)
      (list* (llvm:const-int (llvm:int64-type) 0) ; tag
             (loop with constructor = (first (type:constructors type))
                   for field in (type:fields constructor)
                   for i from 0
                   for stref = (llvm:build-extract-value
                                *builder* value i "")
                   appending (wordify field stref)))
      (loop for i from 0 below (nwords type)
            collecting (llvm:build-extract-value *builder* value i ""))))
(defmethod construct ((layout dumb-layout) adt constructor &rest args)
  (let* ((str (llvm:undef (ltype layout)))
         (fconstructors (type:constructors adt))
         (rconstructor (find (type:name constructor) fconstructors
                             :key #'type:name))
         (tag (position rconstructor fconstructors)))
    (assert (not (null tag)))
    (setf str (llvm:build-insert-value *builder* str (llvm:const-int
                                                      (llvm:int64-type)
                                                      tag) 0 "tag"))
    (loop with i = 1
          for arg in args
          for field in (type:fields rconstructor)
          do (loop for word in (wordify field arg)
                   do (setf str
                            (llvm:build-insert-value *builder* str word i ""))
                      (incf i)))
    str))

(defmethod translate-ast ((ast ast:construct) env)
  (let* ((type (ast:type ast))
         (layout (layout type))
         (args (loop for arg in (ast:args ast)
                     for value = (translate-ast arg env)
                     when (null value)
                       do (return-from translate-ast nil)
                     else collect value)))
    (apply #'construct layout type (ast:constructor ast) args)))

(defgeneric translate-case (layout value ast env))
(defmethod translate-case ((layout direct-layout) value ast env)
  ;; Don't need to do any switch; just bind the variables and go
  (let* ((!p (ast:case!p ast))
         (clause (first (ast:clauses ast)))
         (variables (ast:variables clause))
         (fieldvals
           (if !p
               (loop with zero = (llvm:const-int (llvm:int64-type) 0)
                     for i from 0 below (length variables)
                     ;; struct GEP has to have i32 const indices
                     ;; per the LLVM language reference manual
                     for c = (llvm:const-int (llvm:int32-type) i)
                     collect (llvm:build-in-bounds-gep
                              ;; zero per GEP rules about pointers
                              *builder* value (list zero c) ""))
               (loop for i from 0 below (length variables)
                     collect (llvm:build-extract-value
                              *builder* value i ""))))
         (new-env (augment env (mapcar #'cons variables fieldvals))))
    (translate-ast (ast:body clause) new-env)))

(defun make-block (name) (llvm:append-basic-block *function* name))

(defgeneric dewordify (type words))
(defmethod dewordify ((type type:int) words)
  (if (< (type:int-type-length type) 64)
      (llvm:build-trunc *builder* (first words) (type->llvm type) "")
      (first words)))
(defmethod dewordify ((type type:pointer) words)
  (llvm:build-int-to-pointer *builder* (first words) (type->llvm type) ""))
(defmethod dewordify ((type type:adt) words)
  (if (direct-layout-adt-p type)
      (let ((st (llvm:undef (type->llvm type)))
            (words (rest words)) ; ignore tag
            (constructor (first (type:constructors type))))
        (loop for i from 0
              for field in (type:fields constructor)
              for loword = 0 then (+ loword hiword)
              for hiword = (+ loword (nwords field))
              for val = (dewordify field (subseq words loword hiword))
              do (setf st (llvm:build-insert-value *builder* st val i "")))
        st)
      (let ((st (llvm:undef (type->llvm type))))
        (loop for i from 0
              for word in words
              do (setf st (llvm:build-insert-value *builder* st word i "")))
        st)))

(defun dumb-case-env (env constructor vars words)
  (let ((values
          (loop for field in (type:fields constructor)
                for loword = 0 then (+ loword hiword) ; previous iter's hiword
                for hiword = (+ loword (nwords field))
                collect (dewordify field (subseq words loword hiword)))))
    (augment env (mapcar #'cons vars values))))
(defmethod translate-case ((layout dumb-layout) value ast env)
  (when (ast:case!p ast) (error "Not implemented yet"))
  (let* ((clauses (ast:clauses ast))
         (tag (llvm:build-extract-value *builder* value 0 "tag"))
         (type (ast:type (ast:value ast)))
         (words (loop for i from 1 below (nwords type)
                      collect (llvm:build-extract-value *builder* value i "")))
         (default (make-block "case-unreachable"))
         (merge (make-block "case-merge"))
         (sw (llvm:build-switch *builder* tag default (length clauses))))
    (llvm:position-builder-at-end *builder* default)
    (llvm:build-unreachable *builder*)
    (llvm:position-builder-at-end *builder* merge)
    (let ((phi (llvm:build-phi *builder* (type->llvm (ast:type ast))
                               "case-result")))
      (loop for clause in clauses
            for vars = (ast:variables clause)
            ;; KLUDGE: we use the constructors from the type so that they are
            ;; fully substituted, rather than from the clauses.
            for constructor in (type:constructors type)
            for bname = (concatenate 'string
                                     "case-"
                                     (string-downcase
                                      (symbol-name (hare:name constructor))))
            for block = (make-block bname)
            for new-env = (progn
                            (llvm:position-builder-at-end *builder* block)
                            (dumb-case-env env constructor vars words))
            for cv = (translate-ast (ast:body clause) new-env)
            for i from 0
            do (llvm:add-case sw (llvm:const-int (llvm:int64-type) i) block)
            unless (null cv)
              do (llvm:build-br *builder* merge)
                 (llvm:add-incoming phi (list cv) (list block)))
      (llvm:position-builder-at-end *builder* merge)
      phi)))

(defmethod translate-ast ((ast ast:case) env)
  (let* ((value (ast:value ast))
         (lvalue (or (translate-ast value env)
                     (return-from translate-ast nil)))
         (vtype (ast:type value))
         (rvtype (if (ast:case!p ast)
                     (type:pointer-type-underlying vtype)
                     vtype))
         (layout (layout rvtype)))
    (translate-case layout lvalue ast env)))

;;; TODO: pointer-load and pointer-store will be much more involved when they're
;;; layout dependent; essentially doing some semiarbitrary mapping
(defmethod translate-ast ((ast ast:primitive) env)
  (ecase (ast:name ast)
    ((hare::!)
     (llvm:build-load *builder* (translate-ast (first (ast:args ast)) env) ""))
    ((hare::set!)
     (llvm:build-store *builder*
                       (translate-ast (second (ast:args ast)) env)
                       (translate-ast (first (ast:args ast)) env))
     ;; FIXME: actually return inert
     :fixme)))

(defmethod translate-ast ((ast ast:with) env)
  (let ((nbytes (ast:nbytes ast)))
    (assert (typep (ast:type nbytes) 'type:int))
    (let ((nbytes (translate-ast nbytes env)))
      (llvm:build-array-alloca *builder* (llvm:int8-type) nbytes
                               (symbol-name (ast:name (ast:variable ast))))
      (translate-ast (ast:body ast) env))))

(defmethod translate-ast ((ast ast:literal) env)
  (declare (ignore env))
  (translate-literal (ast:initializer ast)))

(defun gepify (value)
  (let ((zero (llvm:const-int (llvm:int64-type) 0)))
    (llvm:build-in-bounds-gep *builder* value (list zero zero) "darr")))

(defun maybe-gepify (value type)
  (if (and (typep type 'type:pointer)
           (typep (type:pointer-type-underlying type) 'type:arrayt))
      (gepify value)
      value))

(defmethod translate-ast ((ast ast:reference) env)
  (maybe-gepify (lookup (ast:variable ast) env) (ast:type ast)))

(defmethod translate-ast ((ast ast:bind) env)
  (let* ((value (translate-ast (ast:value ast) env))
         (_ (unless value (return-from translate-ast nil)))
         (new-env (augment env (list (cons (ast:variable ast) value)))))
    (declare (ignore _))
    (translate-ast (ast:body ast) new-env)))

;;;

(defgeneric translate-literal (literal))

(defmethod translate-literal ((literal ast:integer-initializer))
  (llvm:const-int (translate-type (ast:type literal))
                  (ast:value literal)))
