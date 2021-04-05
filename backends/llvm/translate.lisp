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

(defgeneric declare-variable (type &key name))

(defmethod declare-variable ((type hare:fun) &key (name ""))
  (let ((type (translate-type type)))
    (llvm:add-function *module* name type)))

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
            (loop for mani in (hare::manifestations manifest)
                  for name = (hare:name mani)
                  for var = (hare:variable mani)
                  for initializer = (hare:initializer mani)
                  for type = (hare::type initializer)
                  for val = (declare-variable type :name name)
                  collect (cons var val))))
         (env (make-instance 'env :bindings binds)))
    (loop for mani in (hare::manifestations manifest)
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

(defmethod translate-ast ((ast reference) env) (lookup (variable ast) env))

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
