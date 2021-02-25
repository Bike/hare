(in-package #:hare)

(defun parse-toplevel (pre-module expr)
  (if (consp expr)
      (cl:case (car expr)
        ((defconstant) (parse-defconstant pre-module expr))
        ((defvar) (parse-defvar pre-module expr))
        ((defadt) (parse-defadt pre-module expr))
        ((deftype) (parse-deftype pre-module expr))
        ((declaim) (parse-declaim pre-module expr))
        ((defmacro) (parse-defmacro pre-module expr))
        ((define-symbol-macro) (parse-define-symbol-macro pre-module expr))
        ((define-tl-macro) (parse-define-tl-macro pre-module expr))
        (otherwise
         (let* ((tlexpanders (tlexpanders pre-module))
                (pair (assoc (car expr) tlexpanders :test #'eq)))
           (if pair
               (parse-toplevel pre-module (funcall (cdr pair) expr))
               (make-instance 'tlunknown
                 :waiting-on-tlop (car expr)
                 :expr expr)))))
      (error "Syntax error on top-level expression ~a" expr)))

(defun add-1-TL-to-pre-module (pre-module expr)
  (let ((new (parse-toplevel pre-module expr)))
    (push new (toplevels pre-module))
    (loop with worklist = (list new)
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
         (info (make-instance 'constant-info :initializer literal)))
    (setf (lookup name (environment pre-module)) info
          (toplevels pre-module) (delete tl (toplevels pre-module) :test #'eq))
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
         (info (make-instance 'variable-info
                 :variable (make-variable name))))
    (setf (lookup name (environment pre-module)) info
          (toplevels pre-module) (delete tl (toplevels pre-module) :test #'eq))
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
           (augment-type-env type-env (mapcar #'name params) params))
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
                           :name cname :fields fields))))
    (setf (constructors pre-def) constructors
          (toplevels pre-module) (delete tl (toplevels pre-module) :test #'eq))
    (finish-adt-def pre-def type-env)
    (append (phase0-type-dependencies pre-module name)
            (loop for cname in cnames
                  nconc (phase0-initop-dependencies pre-module cname)))))
