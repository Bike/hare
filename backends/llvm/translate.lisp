(in-package #:hare-llvm)

(defvar *module*)

(defmacro with-module ((&rest args) &body body)
  `(llvm:with-object (*module* llvm:module ,@args)
     (setf (llvm:target *module*)
           "x86_64-pc-linux-gnu")
     ,@body))

;;;

;; locals and globals are separate, as globals, being polymorphic, need a type.
;; locals are just an alist of (variable . llvm-value)
;; globals are an alist of (variable . (type map initializer llvm-value))
(defclass env ()
  ((%locals :initarg :locals :initform nil :accessor locals)
   (%globals :initarg :globals :accessor globals)))

(defun augment (env pairs)
  (make-instance 'env
    :globals (globals env)
    :locals (append pairs (locals env))))

(defun %lookup (variable type env)
  (let ((localpair (assoc variable (locals env))))
    (if localpair
        (cdr localpair)
        (let ((globalpair (assoc variable (globals env))))
          (if globalpair
              (let* ((type (subst-type *type-map*
                                       (hare:pointer-type-underlying type)))
                     (typepair (assoc type (rest globalpair)
                                      :test #'hare::type=)))
                (if typepair
                    (fourth typepair)
                    nil))
              nil)))))

(defun lookup (variable type env)
  (or (%lookup variable type env)
      (error "BUG in translate: Unknown variable ~a" variable)))

;;;

(defvar *type-map*)

(defun bindings->llvm (bindings exports externs)
  (let ((global-binds
          (append
           (loop for (var type cname) in externs
                 collecting
                 (cons var
                       (list
                        (list type nil nil (extern-global type :name cname)))))
           (loop for (var . things) in bindings
                 ;; Get a list of exports for this var.
                 for var-exports = (remove var exports
                                           :test-not #'eq :key #'first)
                 for name = (string-downcase (symbol-name (name var)))
                 collecting
                 (cons var
                       (loop for (type . initializer) in things
                             for export = (find type var-exports
                                                :test #'hare::type=
                                                :key #'second)
                             for cname = (third export)
                             for mname = (or cname (mangle name type))
                             for *type-map*
                               = (unify (hare::type initializer) type)
                             for global = (initializer-global initializer
                                                              :name mname)
                             collect (list type *type-map*
                                           initializer global)))))))
    (loop with global-env = (make-instance 'env :globals global-binds)
          for (var . things) in global-binds
          do (loop for (type *type-map* initializer global) in things
                   unless (null initializer) ; extern
                     do (translate-initializer initializer global
                                               global-env)))))

(defun mangle (name type)
  ;; FIXME
  (format nil "~a_~a" name (hare::unparse-type type)))

(defun translate-type (type)
  (type->llvm (subst-type *type-map* type)))

;;;


(defvar *builder*)

(defmacro with-builder ((&rest args) &body body)
  `(llvm:with-object (*builder* llvm:builder ,@args) ,@body))

;;;
;; Construct and return an LLVMValueRef for the extern global.
(defgeneric extern-global (type &key name))

(defmethod extern-global ((type fun) &key (name ""))
  (let ((type (type->llvm type)))
    (llvm:add-function *module* name type)))

;;;

;; Construct and return an LLVMValueRef for the global.
(defgeneric initializer-global (initializer &key name))

(defmethod initializer-global ((initializer hare::lambda-initializer)
                               &key (name ""))
  (let ((type (translate-type (hare::type initializer))))
    (llvm:add-function *module* name type)))

;;;

(defgeneric translate-initializer (initializer valref global-env))

(defmethod translate-initializer ((lambda-initializer hare::lambda-initializer)
                                  function global-env)
  (let* ((params (hare::params lambda-initializer))
         (body (body lambda-initializer))
         (lparams (llvm:params function))
         (env (augment global-env (mapcar #'cons params lparams)))
         (entry (llvm:append-basic-block function "entry")))
    (with-builder ()
      (llvm:position-builder-at-end *builder* entry)
      (let ((r (translate-ast body env)))
        (unless (null r)
          (llvm:build-ret *builder* r)))
      function)))

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

(defmethod translate-ast ((ast literal) env)
  (declare (ignore env))
  (translate-literal (initializer ast)))

(defmethod translate-ast ((ast reference) env)
  (lookup (variable ast) (hare::type ast) env))

(defmethod translate-ast ((ast branch) env)
  (let ((condition (translate-ast (test ast) env))
        (thenb (llvm:create-basic-block "then"))
        (elseb (llvm:create-basic-block "else"))
        (mergeb (llvm:create-basic-block "if")))
    (llvm:build-cond-br *builder* condition thenb elseb)
    (let* ((then
             (progn
               (llvm:position-builder-at-end *builder* thenb)
               (translate-ast (then ast) env)))
           (_ (unless (null then)
                (llvm:build-br *builder* mergeb)))
           (else
             (progn
               (llvm:position-builder-at-end *builder* elseb)
               (translate-ast (else ast) env)))
           (_2 (unless (null else)
                 (llvm:build-br *builder* mergeb))))
      (declare (ignore _ _2))
      (cond
        ((and then else)
         (llvm:position-builder-at-end *builder* mergeb)
         (let ((phi (llvm:build-phi
                     *builder* (translate-type (hare::type ast)) "if")))
           (llvm:add-incoming phi (list then else) (list thenb elseb))
           phi))
        (then)
        (else)
        (t nil)))))

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
