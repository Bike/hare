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

(defmethod declare-variable ((type hare:fun) &key (name "") initializer)
  (declare (ignore initializer))
  (let ((type (translate-type type)))
    (llvm:add-function *module* name type)))

(defmethod declare-variable ((type hare:arrayt) &key (name "") initializer)
  (let* ((etype (translate-type (hare:arrayt-element-type type)))
         (len (if initializer (length (hare::elements initializer)) 0))
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
                  for type = (hare::type initializer)
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

(defmethod translate-initializer ((lambda-initializer hare::lambda-initializer)
                                  *function* global-env)
  (let* ((params (hare::params lambda-initializer))
         (body (body lambda-initializer))
         (lparams (llvm:params *function*))
         (env (augment global-env (mapcar #'cons params lparams)))
         (entry (llvm:append-basic-block *function* "entry")))
    (with-builder ()
      (llvm:position-builder-at-end *builder* entry)
      (let ((r (translate-ast body env)))
        (unless (null r)
          (llvm:build-ret *builder* r)))
      *function*)))

(defmethod translate-initializer ((init hare::array-initializer)
                                  var global-env)
  (let* ((etype (type->llvm (hare:arrayt-element-type (hare::type init))))
         (const (llvm:const-array etype
                                  ;; KLUDGE?
                                  (mapcar #'translate-literal
                                          (hare::elements init)))))
    (llvm:set-initializer var const)
    var))

;;;

(defgeneric translate-ast (ast env)
  (:documentation "Generate LLVM-IR for the given AST.
Return an LLVMValueRef for the result, or NIL if there isn't one
(e.g. if there is an escape)."))

(defmethod translate-ast ((ast seq) env)
  (let ((asts (asts ast)))
    (if (null asts)
        (error "TODO :(")
        (loop with result = nil
              for ast in asts
              do (setf result (translate-ast ast env))
              when (null result) return nil
              finally (return result)))))

(defmethod translate-ast ((ast call) env)
  (let* ((callee (translate-ast (callee ast) env))
         (_ (when (null callee) (return-from translate-ast nil)))
         (args (loop for arg in (args ast)
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
(defmethod wordify ((type hare:int) value)
  (list (if (< (hare:int-type-length type) 64)
            (llvm:build-z-ext *builder* value (llvm:int64-type) "")
            value)))
(defmethod wordify ((type hare:pointer) value)
  (list (llvm:build-pointer-to-int *builder* value (llvm:int64-type) "")))
(defmethod wordify ((type hare:adt) value)
  (if (direct-layout-adt-p type)
      (list* (llvm:const-int (llvm:int64-type) 0) ; tag
             (loop with constructor = (first (hare:constructors type))
                   for field in (hare::fields constructor)
                   for i from 0
                   for stref = (llvm:build-extract-value
                                *builder* value i "")
                   appending (wordify field stref)))
      (loop for i from 0 below (nwords type)
            collecting (llvm:build-extract-value *builder* value i ""))))
(defmethod construct ((layout dumb-layout) adt constructor &rest args)
  (let* ((str (llvm:undef (ltype layout)))
         (fconstructors (hare:constructors adt))
         (rconstructor (find (hare:name constructor) fconstructors
                             :key #'hare:name))
         (tag (position rconstructor fconstructors)))
    (assert (not (null tag)))
    (setf str (llvm:build-insert-value *builder* str (llvm:const-int
                                                      (llvm:int64-type)
                                                      tag) 0 "tag"))
    (loop with i = 1
          for arg in args
          for field in (hare::fields rconstructor)
          do (loop for word in (wordify field arg)
                   do (setf str
                            (llvm:build-insert-value *builder* str word i ""))
                      (incf i)))
    str))

(defmethod translate-ast ((ast construct) env)
  (let* ((type (hare::type ast))
         (layout (layout type))
         (args (loop for arg in (args ast)
                     for value = (translate-ast arg env)
                     when (null value)
                       do (return-from translate-ast nil)
                     else collect value)))
    (apply #'construct layout type (hare::constructor ast) args)))

(defgeneric translate-case (layout value ast env))
(defmethod translate-case ((layout direct-layout) value ast env)
  ;; Don't need to do any switch; just bind the variables and go
  (let* ((!p (hare::case!p ast))
         (clause (first (hare::clauses ast)))
         (variables (hare::variables clause))
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
    (translate-ast (hare:body clause) new-env)))

(defun make-block (name) (llvm:append-basic-block *function* name))

(defgeneric dewordify (type words))
(defmethod dewordify ((type hare:int) words)
  (if (< (hare:int-type-length type) 64)
      (llvm:build-trunc *builder* (first words) (type->llvm type) "")
      (first words)))
(defmethod dewordify ((type hare:pointer) words)
  (llvm:build-int-to-pointer *builder* (first words) (type->llvm type) ""))
(defmethod dewordify ((type hare:adt) words)
  (if (direct-layout-adt-p type)
      (let ((st (llvm:undef (type->llvm type)))
            (words (rest words)) ; ignore tag
            (constructor (first (hare:constructors type))))
        (loop for i from 0
              for field in (hare::fields constructor)
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
          (loop for field in (hare::fields constructor)
                for loword = 0 then (+ loword hiword) ; previous iter's hiword
                for hiword = (+ loword (nwords field))
                collect (dewordify field (subseq words loword hiword)))))
    (augment env (mapcar #'cons vars values))))
(defmethod translate-case ((layout dumb-layout) value ast env)
  (when (hare::case!p ast) (error "Not implemented yet"))
  (let* ((clauses (hare::clauses ast))
         (tag (llvm:build-extract-value *builder* value 0 "tag"))
         (type (hare::type (hare:value ast)))
         (words (loop for i from 1 below (nwords type)
                      collect (llvm:build-extract-value *builder* value i "")))
         (default (make-block "case-unreachable"))
         (merge (make-block "case-merge"))
         (sw (llvm:build-switch *builder* tag default (length clauses))))
    (llvm:position-builder-at-end *builder* default)
    (llvm:build-unreachable *builder*)
    (llvm:position-builder-at-end *builder* merge)
    (let ((phi (llvm:build-phi *builder* (type->llvm (hare::type ast))
                               "case-result")))
      (loop for clause in clauses
            for vars = (hare::variables clause)
            ;; KLUDGE: we use the constructors from the type so that they are
            ;; fully substituted, rather than from the clauses.
            for constructor in (hare:constructors type)
            for bname = (concatenate 'string
                                     "case-"
                                     (string-downcase
                                      (symbol-name (hare:name constructor))))
            for block = (make-block bname)
            for new-env = (progn
                            (llvm:position-builder-at-end *builder* block)
                            (dumb-case-env env constructor vars words))
            for cv = (translate-ast (hare:body clause) new-env)
            for i from 0
            do (llvm:add-case sw (llvm:const-int (llvm:int64-type) i) block)
            unless (null cv)
              do (llvm:build-br *builder* merge)
                 (llvm:add-incoming phi (list cv) (list block)))
      (llvm:position-builder-at-end *builder* merge)
      phi)))

(defmethod translate-ast ((ast case) env)
  (let* ((value (hare:value ast))
         (lvalue (or (translate-ast value env)
                     (return-from translate-ast nil)))
         (vtype (hare::type value)) (layout (layout vtype)))
    (translate-case layout lvalue ast env)))

(defmethod translate-ast ((ast literal) env)
  (declare (ignore env))
  (translate-literal (initializer ast)))

(defun gepify (value)
  (let ((zero (llvm:const-int (llvm:int64-type) 0)))
    (llvm:build-in-bounds-gep *builder* value (list zero zero) "darr")))

(defun maybe-gepify (value type)
  (if (and (typep type 'hare::pointer)
           (typep (hare:pointer-type-underlying type) 'hare::arrayt))
      (gepify value)
      value))

(defmethod translate-ast ((ast reference) env)
  (maybe-gepify (lookup (variable ast) env) (hare::type ast)))

(defmethod translate-ast ((ast bind) env)
  (let* ((value (translate-ast (value ast) env))
         (_ (unless value (return-from translate-ast nil)))
         (new-env (augment env (list (cons (variable ast) value)))))
    (declare (ignore _))
    (translate-ast (body ast) new-env)))

;;;

(defgeneric translate-literal (literal))

(defmethod translate-literal ((literal hare::integer-initializer))
  (llvm:const-int (translate-type (hare::type literal))
                  (value literal)))
