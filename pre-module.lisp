(in-package #:hare)

(defclass pre-module ()
  ((%environment :initarg :env :reader environment :type environment)
   (%type-env :initarg :type-env :reader type-env :type type-env)
   ;; An alist from VARIABLEs to INITIALIZERs.
   (%varbinds :initform nil :accessor varbinds :type list)
   ;; An alist from names to toplevel macro functions.
   (%tlexpanders :initform nil :accessor tlexpanders :type list)
   ;; A list of toplevel forms with unknown head
   ;; (i.e. that are waiting on a tlmacro definition)
   (%unknown-toplevels :initform nil :accessor unknown-toplevels :type list)
   ;; An alist from variableish names to initializers requiring reparsing once
   ;; those names are defined
   (%unknown-initnames :initform nil :accessor unknown-initnames :type list)
   ;; ditto but with ASTs
   (%unknown-astnames :initform nil :accessor unknown-astnames :type list)
   ;; Alist from constructor names to initializers requiring etc.
   (%unknown-initcons :initform nil :accessor unknown-initcons :type list)
   ;; ditto but with ASTs
   (%unknown-astcons :initform nil :accessor unknown-astcons :type list)
   ;; An alist from type names to types requiring reparsing once those names
   ;; are defined
   (%unknown-types :initform nil :accessor unknown-types :type list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unknowns and resolving them

(defun completep (pre-module)
  (and (null (unknown-toplevels pre-module))
       (null (unknown-initnames pre-module))
       (null (unknown-astnames pre-module))
       (null (unknown-initcons pre-module))
       (null (unknown-astcons pre-module))
       (null (unknown-types pre-module))))

;;; Call the thunk in such a way that the unknown-whatever conditions are
;;; handled by recording the unknown and then continuing.
;;; Returns the thunk's results.
;;; This could maybe be broken up into types and initializers and bla bla but
;;; it shouldn't really matter.
(defun call-with-unknowns-handled (pre-module thunk)
  (handler-bind
      ((unknown-variable
         (lambda (e)
           (let ((placeholder (placeholder e)))
             (etypecase placeholder
               (ast:initializer (push (cons (name e) placeholder)
                                      (unknown-initnames pre-module)))
               (ast:ast (push (cons (name e) placeholder)
                              (unknown-astnames pre-module)))))
           (continue e)))
       (unknown-constructor
         (lambda (e)
           (let ((placeholder (placeholder e)))
             (etypecase placeholder
               (ast:initializer (push (cons (name e) placeholder)
                                      (unknown-initcons pre-module)))
               (ast:ast (push (cons (name e) placeholder)
                              (unknown-astcons pre-module)))))
           (continue e)))
       ;; At some point we might get these with initializers and ASTs as well?
       (unknown-type
         (lambda (e)
           (let ((placeholder (placeholder e)))
             (etypecase placeholder
               (type:type (push (cons (name e) placeholder)
                                (unknown-types pre-module)))))
           (continue e))))
    (funcall thunk)))

;;; macro version of above
(defmacro with-unknowns-handled ((pre-module) &body body)
  `(call-with-unknowns-handled ,pre-module (lambda () (progn ,@body))))

;;; If this is T, adding a new toplevel will immediately resolve any toplevels
;;; waiting on a name it defines. Without this, you'll have to do resolutions
;;; yourself. TODO: Function to apply all available resolutions.
(defvar *online-resolution* t)

(defun resolve-variable (pre-module name)
  (loop with env = (environment pre-module)
        with type-env = (type-env pre-module)
        for pair in (unknown-initnames pre-module)
        for (v . initializer) = pair
        when (eq v name)
          do (with-unknowns-handled (pre-module)
               (reparse-initializer initializer env type-env))
          and collect pair into resolved
        finally (setf (unknown-initnames pre-module)
                      (set-difference (unknown-initnames pre-module)
                                      resolved
                                      :test #'eq)))
  (loop with env = (environment pre-module)
        with type-env = (type-env pre-module)
        for pair in (unknown-astnames pre-module)
        for (v . ast) = pair
        when (eq v name)
          do (with-unknowns-handled (pre-module)
               (reparse-ast ast env type-env))
          and collect pair into resolved
        finally (setf (unknown-astnames pre-module)
                      (set-difference (unknown-astnames pre-module)
                                      resolved
                                      :test #'eq)))
  (values))

(defun resolve-constructor (pre-module name)
  (loop with env = (environment pre-module)
        with type-env = (type-env pre-module)
        for pair in (unknown-initcons pre-module)
        for (v . initializer) = pair
        when (eq v name)
          do (with-unknowns-handled (pre-module)
               (reparse-initializer initializer env type-env))
          and collect pair into resolved
        finally (setf (unknown-initcons pre-module)
                      (set-difference (unknown-initcons pre-module)
                                      resolved
                                      :test #'eq)))
  (loop with env = (environment pre-module)
        with type-env = (type-env pre-module)
        for pair in (unknown-astcons pre-module)
        for (v . ast) = pair
        when (eq v name)
          do (with-unknowns-handled (pre-module)
               (reparse-ast ast env type-env))
          and collect pair into resolved
        finally (setf (unknown-astcons pre-module)
                      (set-difference (unknown-astcons pre-module)
                                      resolved
                                      :test #'eq)))
  (values))

(defun resolve-type (pre-module name)
  (loop with type-env = (type-env pre-module)
        for pair in (unknown-types pre-module)
        for (v . type) = pair
        when (eq v name)
          do (with-unknowns-handled (pre-module)
               (reparse-type type type-env))
          and collect pair into resolved
        finally (setf (unknown-types pre-module)
                      (set-difference (unknown-types pre-module) resolved
                                      :test #'eq)))
  (values))

(defun resolve-toplevel-macro (pre-module name)
  (loop for expr in (unknown-toplevels pre-module)
        when (eq (car expr) name)
          do (parse-toplevel pre-module expr)
          and collect expr into resolved
        finally (setf (unknown-toplevels pre-module)
                      (set-difference (unknown-toplevels pre-module) resolved
                                      :test #'eq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsing

(defun parse-pre-module (toplevel-exprs
                         &optional (env (make-stdenv)) (tenv (make-type-env)))
  (let ((prem (make-instance 'pre-module :env env :type-env tenv)))
    (loop for expr in toplevel-exprs do (parse-toplevel prem expr))
    prem))

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
        ((define-toplevel-macro) (parse-define-tl-macro pre-module expr))
        ((seq) (loop for tl in (cdr expr)
                     do (parse-toplevel pre-module tl)))
        (otherwise
         (let* ((tlexpanders (tlexpanders pre-module))
                (pair (assoc (car expr) tlexpanders :test #'eq)))
           (if pair
               (parse-toplevel pre-module (funcall (cdr pair) expr))
               (push expr (unknown-toplevels pre-module))))))
      (error "Syntax error on top-level expression ~a" expr)))

(defun parse-defconstant (pre-module expr)
  (destructuring-bind (name lexpr) (rest expr)
    ;; FIXME: Handle unknown constructors, variables?
    (let* ((environment (environment pre-module))
           (literal (parse-literal lexpr environment (type-env pre-module)))
           (old-info (lookup name environment)))
      (etypecase old-info
        (null (setf (lookup name environment)
                    (make-instance 'constant-info :initializer literal)))))
    (when *online-resolution* (resolve-variable pre-module name)))
  (values))

(defun parse-defvar (pre-module expr)
  (destructuring-bind (name &optional (initializer 'undef)) (rest expr)
    (let* ((env (environment pre-module))
           (initializer (with-unknowns-handled (pre-module)
                         (parse-initializer initializer env
                                            (type-env pre-module))))
           (old-info (lookup name env))
           (variable (ast:make-variable name)))
      (push (cons variable initializer) (varbinds pre-module))
      (etypecase old-info
        (variable-info)
        (null (setf (lookup name env) (make-instance 'variable-info
                                        :variable variable))))
      (when *online-resolution* (resolve-variable pre-module name))))
  (values))

(defun augment-type-env-with-tvars (type-env tvars)
  (augment-type-env type-env (loop for tvar in tvars
                                   collect (list (type:name tvar) () tvar))))

(defun parse-defadt (pre-module expr)
  (destructuring-bind (name tvars &rest multiplicands) (rest expr)
    (let* ((type-env (type-env pre-module))
           (tvars (mapcar #'type:make-tvar tvars))
           (def (make-instance 'adt-def :name name :tvars tvars))
           ;; Want to do this before parsing fields, in case it's recursive.
           (_ (add-adt-def def type-env))
           (internal-type-env (augment-type-env-with-tvars type-env tvars))
           (constructors
             (loop for (cname . field-exprs) in multiplicands
                   for fields
                     = (loop for fexpr in field-exprs
                             collect (with-unknowns-handled (pre-module)
                                       (parse-type fexpr internal-type-env)))
                   collect (make-instance 'constructor
                             :name cname :adt-def def :fields fields))))
      (declare (ignore _))
      (setf (type:constructors def) constructors)
      (loop for constructor in constructors
            do (add-adt-constructor constructor type-env))
      (when *online-resolution*
        (resolve-type pre-module name)
        (loop for constructor in constructors
              do (resolve-constructor pre-module (type:name constructor))))))
  (values))

(defun parse-deftype (pre-module expr)
  (destructuring-bind (name (&rest tvarnames) utype) (rest expr)
    (let* ((tvars (mapcar #'type:make-tvar tvarnames))
           (type-env (type-env pre-module))
           (internal-type-env (augment-type-env-with-tvars type-env tvars))
           (expansion (with-unknowns-handled (pre-module)
                        (parse-type utype internal-type-env))))
      (add-alias name tvars expansion type-env))
    (when *online-resolution* (resolve-type pre-module name)))
  (values))

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

(defun parse-defmacro (pre-module expr)
  (destructuring-bind (name mll &body body) (rest expr)
    (let* ((expander (coerce (parse-macro name mll body) 'function))
           (info (make-instance 'macro-info :expander expander))
           (env (environment pre-module)))
      (etypecase (lookup name env)
        (null (setf (lookup name env) info))))
    (when *online-resolution* (resolve-variable pre-module name)))
  (values))

(defun parse-define-symbol-macro (pre-module expr)
  (destructuring-bind (name expansion) (rest expr)
    (let* ((expander (lambda (form env) (declare (ignore form env)) expansion))
           (info (make-instance 'symbol-macro-info :expander expander))
           (env (environment pre-module)))
      (etypecase (lookup name env)
        (null (setf (lookup name env) info))))
    (when *online-resolution* (resolve-variable pre-module name)))
  (values))

(defun parse-define-tl-macro (pre-module expr)
  (destructuring-bind (name mll &body body) (rest expr)
    (let ((expander (coerce (parse-macro name mll body) 'function)))
      (when (assoc name (tlexpanders pre-module))
        (error "Duplicate tl macro ~a" name))
      (push (cons name expander) (tlexpanders pre-module)))
    (when *online-resolution* (resolve-toplevel-macro pre-module name)))
  (values))

(defun parse-declaim (pre-module expr)
  (loop for declamation in (rest expr)
        do (ecase (first declamation)
             ((type) (parse-type-declamation pre-module declamation))
             ((variable) (parse-variable-declamation pre-module declamation)))))

(defun parse-type-declamation (pre-module decl)
  (destructuring-bind (tvarnames type &rest names) (rest decl)
    (let* ((tvars (mapcar #'type:make-tvar tvarnames))
           (type-env (type-env pre-module))
           (internal-type-env (augment-type-env-with-tvars type-env tvars))
           (type (with-unknowns-handled (pre-module)
                   (type:make-pointer (parse-type type internal-type-env))))
           (env (environment pre-module)))
      (dolist (name names)
        (let ((info (lookup name env)))
          (etypecase info
            (null ; treat type declamations as implying a variable declamation
             (setf info (make-instance 'variable-info
                          :variable (ast:make-variable name)))
             (setf (lookup name env) info)
             (when *online-resolution* (resolve-variable pre-module name)))
            (variable-info))
          (setf (declared-type info) (type:schema type tvars))))))
  (values))

(defun parse-variable-declamation (pre-module decl)
  (let ((env (environment pre-module)))
    (dolist (name (rest decl))
      (etypecase (lookup name env)
        (null
         (setf (lookup name env) (make-instance 'variable-info
                                   :variable (ast:make-variable name)))
         (when *online-resolution* (resolve-variable pre-module name)))
        (variable-info))))
  (values))
