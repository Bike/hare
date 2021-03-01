(in-package #:hare)

(defun parse-toplevel (pre-module expr)
  (if (consp expr)
      (cl:case (car expr)
        ((defconstant) (list (parse-defconstant pre-module expr)))
        ((defvar) (list (parse-defvar pre-module expr)))
        ((defadt) (list (parse-defadt pre-module expr)))
        ((deftype) (list (parse-deftype pre-module expr)))
        ((declaim) (parse-declaim pre-module expr))
        ((defmacro) (list (parse-defmacro pre-module expr)))
        ((define-symbol-macro)
         (list (parse-define-symbol-macro pre-module expr)))
        ((define-toplevel-macro)
         (list (parse-define-tl-macro pre-module expr)))
        ((seq) (loop for tl in (cdr expr)
                     nconcing (parse-toplevel pre-module tl)))
        (otherwise
         (let* ((tlexpanders (tlexpanders pre-module))
                (pair (assoc (car expr) tlexpanders :test #'eq)))
           (if pair
               (parse-toplevel pre-module (funcall (cdr pair) expr))
               (list (make-instance 'tlunknown
                       :waiting-on-tlop (car expr)
                       :expr expr))))))
      (error "Syntax error on top-level expression ~a" expr)))

(defun add-1-TL-to-pre-module (pre-module expr)
  (let ((new (parse-toplevel pre-module expr)))
    (setf (toplevels pre-module) (append new (toplevels pre-module)))
    (loop with worklist = new
          for work = (if worklist (pop worklist) (return))
          for newlist = (phase0parse work pre-module)
          do (setf worklist (nconc newlist worklist))))
  (values))

(defun parse-pre-module (toplevel-exprs
                         &optional (env (make-env nil nil))
                           (tenv (make-type-env)))
  (let ((module (make-instance 'pre-module :env env :type-env tenv)))
    (loop for expr in toplevel-exprs
          do (add-1-TL-to-pre-module module expr))
    module))

(defun phase0-var-dependencies (pre-module name)
  (loop for tl in (toplevels pre-module)
        when (and (typep tl 'waiting-on-vars)
                  (member name (waiting-on-vars tl)))
          collect tl))

(defun phase0-type-dependencies (pre-module name)
  (loop for tl in (toplevels pre-module)
        when (and (typep tl 'waiting-on-types)
                  (member name (waiting-on-types tl)))
          collect tl))

(defun phase0-initop-dependencies (pre-module name)
  (loop for tl in (toplevels pre-module)
        when (and (typep tl 'waiting-on-initops)
                  (member name (waiting-on-initops tl)))
          collect tl))

(defun phase0-tl-dependencies (pre-module name)
  (loop for tl in (toplevels pre-module)
        when (and (typep tl 'tlunknown)
                  (member name (waiting-on-tlop tl)))
          collect tl))

(defgeneric phase0parse (toplevel pre-module))

(defun parse-defconstant (pre-module expr)
  (declare (ignore pre-module))
  (destructuring-bind (name lexpr) (rest expr)
    (make-instance 'tldefconstant :name name :expr lexpr)))

(defmethod phase0parse ((tl tldefconstant) pre-module)
  (setf (waiting-on-vars tl) nil (waiting-on-initops tl) nil)
  (let* ((name (name tl)) (lexpr (expr tl))
         (literal
           (handler-case
               (parse-literal
                lexpr (environment pre-module) (type-env pre-module))
             (variable-unbound (e)
               (push (name e) (waiting-on-vars tl))
               (return-from phase0parse nil))
             (unknown-constructor (e)
               (push (name e) (waiting-on-initops tl))
               (return-from phase0parse nil))))
         ;; success
         (env (environment pre-module))
         (old-info (forgiving-lookup name env)))
    (setf (toplevels pre-module) (delete tl (toplevels pre-module) :test #'eq))
    (etypecase old-info
      (null (setf (lookup name env)
                  (make-instance 'constant-info :initializer literal))))
    (phase0-var-dependencies pre-module name)))

(defun parse-defvar (pre-module expr)
  (declare (ignore pre-module))
  (destructuring-bind (name &optional (initializer nil initializerp))
      (rest expr)
    (make-instance 'tldefvar
      :name name :expr (if initializerp initializer 'undef))))

(defmethod phase0parse ((tl tldefvar) pre-module)
  (setf (waiting-on-vars tl) nil (waiting-on-types tl) nil
        (waiting-on-initops tl) nil)
  (let* ((name (name tl)) (lexpr (expr tl))
         (initializer
           (handler-case
               (parse-initializer lexpr
                                  (environment pre-module)
                                  (type-env pre-module))
             (variable-unbound (e)
               (push (name e) (waiting-on-vars tl))
               (return-from phase0parse nil))
             (unknown-adt (e)
               (push (name e) (waiting-on-types tl))
               (return-from phase0parse nil))
             (unknown-constructor (e)
               (push (name e) (waiting-on-initops tl))
               (return-from phase0parse nil))))
         (variable (make-variable name))
         (env (environment pre-module))
         (old-info (forgiving-lookup name env)))
    (setf (toplevels pre-module) (delete tl (toplevels pre-module) :test #'eq))
    (etypecase old-info
      (variable-info)
      (null (setf (lookup name env) (make-instance 'variable-info
                                      :variable (make-variable name)))))
    (push (cons variable initializer) (varbinds pre-module))
    (phase0-var-dependencies pre-module name)))

(defun parse-defadt (pre-module expr)
  (destructuring-bind (name tvars &rest fields) (rest expr)
    (let ((def (make-instance 'adt-def
                 :name name :tvars (mapcar #'make-tvar tvars))))
      (add-adt-def def (type-env pre-module))
      (make-instance 'tldefadt
        :name name :pre-def def
        :cnames (mapcar #'first fields) :exprs (mapcar #'rest fields)))))

(defmethod phase0parse ((tl tldefadt) pre-module)
  (setf (waiting-on-types tl) nil)
  (let* ((name (name tl)) (pre-def (pre-def tl))
         (cnames (cnames tl)) (exprs (exprs tl))
         (params (tvars pre-def))
         (type-env (type-env pre-module))
         (internal-type-env
           (augment-type-env type-env
                             (loop for param in params
                                   collect (list (name param) () param))))
         (fieldses
           (loop for expr in exprs
                 collect (loop for field in expr
                               collect (handler-case
                                           (parse-type field internal-type-env)
                                         (unknown-adt (e)
                                           (push (name e) (waiting-on-types tl))
                                           (return-from phase0parse nil))))))
         ;; success
         (constructors
           (loop for cname in cnames
                 for fields in fieldses
                 collect (make-instance 'constructor
                           :name cname :adt-def pre-def :fields fields))))
    (setf (constructors pre-def) constructors
          (toplevels pre-module) (delete tl (toplevels pre-module) :test #'eq))
    (loop for constructor in constructors
          do (add-adt-constructor constructor type-env))
    (append (phase0-type-dependencies pre-module name)
            (loop for cname in cnames
                  nconc (phase0-initop-dependencies pre-module cname)))))

(defun parse-deftype (pre-module expr)
  (declare (ignore pre-module))
  (destructuring-bind (name (&rest tvarnames) utype) (rest expr)
    (make-instance 'tldeftype
      :name name :parameters (mapcar #'make-tvar tvarnames) :expr utype)))

(defmethod phase0parse ((tl tldeftype) pre-module)
  (setf (waiting-on-types tl) nil)
  (let* ((name (name tl)) (tvars (parameters tl)) (expr (expr tl))
         (type-env (type-env pre-module))
         (expansion
           (handler-case (parse-type expr type-env)
             (unknown-adt (e)
               (push (name e) (waiting-on-types tl))
               (return-from phase0parse nil)))))
    (add-alias name tvars expansion type-env)
    (setf (toplevels pre-module) (delete tl (toplevels pre-module) :test #'eq))
    (phase0-type-dependencies pre-module name)))

(defun parse-defmacro (pre-module expr)
  (declare (ignore pre-module))
  (destructuring-bind (name macro-lambda-list &body body) (rest expr)
    (make-instance 'tldefmacro :name name :mll macro-lambda-list :expr body)))

(defmacro destructure-mll (macro-lambda-list object body)
  (labels ((bindings (mll argvar)
             (etypecase mll
               (symbol `((,mll ,argvar)))
               (null nil)
               (cons (let ((carvar (gensym "CAR")) (cdrvar (gensym "CDR")))
                       `((,carvar (car ,argvar))
                         ,@(bindings (car mll) carvar)
                         (,cdrvar (cdr ,argvar))
                         ,@(bindings (cdr mll) cdrvar)))))))
    (let ((gobj (gensym "OBJECT")))
      `(let* ((,gobj ,object)
              ,@(bindings macro-lambda-list gobj))
         ,@body))))

(defun parse-macro (name macro-lambda-list body)
  (let ((gform (gensym "FORM")) (genv (gensym "ENV")))
    `(lambda (,gform ,genv)
       (declare (ignore ,genv))
       (block ,name
         (destructure-mll ,macro-lambda-list (cdr ,gform) ,@body)))))

(defmethod phase0parse ((tl tldefmacro) pre-module)
  ;; not dependent on anything
  (let* ((name (name tl)) (mll (mll tl)) (body (expr tl))
         (expander (coerce (parse-macro name mll body) 'function))
         (info (make-instance 'macro-info :expander expander)))
    (setf (lookup name (environment pre-module)) info
          (toplevels pre-module) (delete tl (toplevels pre-module) :test #'eq))
    (phase0-var-dependencies pre-module name)))

(defun parse-define-symbol-macro (pre-module expr)
  (declare (ignore pre-module))
  (destructuring-bind (name expansion) (rest expr)
    (make-instance 'tldefine-symbol-macro :name name :expr expansion)))

(defmethod phase0parse ((tl tldefine-symbol-macro) pre-module)
  ;; also independent
  (let* ((name (name tl)) (expansion (expr tl))
         (expander (lambda (form env) (declare (ignore form env)) expansion))
         (info (make-instance 'symbol-macro-info :expander expander)))
    (setf (lookup name (environment pre-module)) info
          (toplevels pre-module) (delete tl (toplevels pre-module) :test #'eq))
    (phase0-var-dependencies pre-module name)))

(defun parse-define-tl-macro (pre-module expr)
  (declare (ignore pre-module))
  (destructuring-bind (name mll &body body) (rest expr)
    (make-instance 'tldefine-tl-macro :name name :mll mll :expr body)))

(defmethod phase0parse ((tl tldefine-tl-macro) pre-module)
  ;; also independent
  (let* ((name (name tl)) (mll (mll tl)) (body (expr tl))
         (expander (coerce (parse-macro name mll body) 'function)))
    (push (cons name expander) (tlexpanders pre-module))
    (setf (toplevels pre-module) (delete tl (toplevels pre-module) :test #'eq))
    (phase0-tl-dependencies pre-module name)))

(defmethod phase0parse ((tl tlunknown) pre-module)
  (let* ((expr (expr tl)) (op (car expr))
         (pair (assoc op (tlexpanders pre-module))))
    (when pair
      (setf (toplevels pre-module) (delete tl (toplevels pre-module)
                                           :test #'eq))
      (let* ((expander (cdr pair))
             (new (parse-toplevel pre-module (funcall expander expr))))
        (push new (toplevels pre-module))
        (list new)))))

(defun parse-declaim (pre-module expr)
  (declare (ignore pre-module))
  (loop with result = nil
        for declamation in (rest expr)
        nconc (ecase (first declamation)
                ((type)
                 (destructuring-bind (tvarnames type &rest names)
                     (rest declamation)
                   (loop for tvars = (mapcar #'make-tvar tvarnames)
                         for name in names
                         collect (make-instance 'declamation-type
                                   :type type :parameters tvars :name name))))
                ((variable)
                 (loop for name in (rest declamation)
                       collect (make-instance 'declamation-variable
                                 :name name))))))

(defmethod phase0parse ((tl declamation-type) pre-module)
  (setf (waiting-on-vars tl) nil (waiting-on-types tl) nil)
  (let* ((name (name tl)) (params (parameters tl)) (type (type tl))
         (env (environment pre-module))
         (info (handler-case (lookup name env)
                 (variable-unbound (e)
                   (push (name e) (waiting-on-vars tl))
                   (return-from phase0parse nil))))
         (type-env (type-env pre-module))
         (new-type-env
           (augment-type-env type-env
                             (loop for param in params
                                   collect (list (name param) () param))))
         (type (handler-case (parse-type type new-type-env)
                 (unknown-adt (e)
                   (push (name e) (waiting-on-types tl))
                   (return-from phase0parse nil)))))
    (setf (declared-type info) (schema type params)
          (toplevels pre-module) (delete tl (toplevels pre-module) :test #'eq)))
  nil)

(defmethod phase0parse ((tl declamation-variable) pre-module)
  ;; independent
  (let* ((name (name tl)) (env (environment pre-module))
         (old-info (forgiving-lookup name env)))
    (etypecase old-info
      (variable-info)
      (null (setf (lookup name env)
                  (make-instance 'variable-info
                    :variable (make-instance 'variable :name name)))))
    (setf (toplevels pre-module) (delete tl (toplevels pre-module) :test #'eq))
    (phase0-var-dependencies pre-module name)))
