(in-package #:hare)

(defun parse-defadt (adt-def form adt-env)
  (destructuring-bind (defadt name tvars &rest members) form
    (declare (ignore defadt name)) ; handled already
    (let ((env (mapcar #'cons tvars (tvars adt-def))))
      (loop for (constructor . types) in members
            collect constructor into constructors
            collect (loop for type in types
                          collect (parse-type type env adt-env))
              into adt-members
            finally (setf (constructors adt-def) constructors
                          (members adt-def) adt-members))))
  (values))

;;; Input is a list of (defadt name (tvar*) (constructor type*)*) forms.
;;; Return an adt environment.
(defun parse-defadts (forms)
  (loop with adt-env = (make-adt-env)
        for form in forms
        for name = (second form)
        for vars = (third form)
        for tvars = (loop for var in vars
                          collect (make-tvar var))
        for def = (make-instance 'adt-def :name name :tvars tvars)
        collect (cons def form) into todo
        do (add-adt-def def adt-env)
        finally (loop for (def . form) in todo
                      do (parse-defadt def form adt-env))
                (return adt-env)))

;;; Given a list of (defconstant name initializer) forms, an env, and an
;;; adt-env, return a new environment extending the given environment
;;; but with the named constants bound to initializers.
(defun parse-defconstants (defconstants env adt-env)
  ;; constants may refer to each other, but not circularly. So if we hit a
  ;; constant we haven't yet seen while parsing an initializer, we go off
  ;; and parse that constant, then try again.
  (let ((cenv env))
    (labels ((parse-defconstant (form)
               (destructuring-bind (def name init) form
                 (declare (ignore def))
                 ;; head off recursion
                 (setf defconstants (delete form defconstants :test #'eq))
                 (tagbody
                  try-again
                    (let ((init
                            (handler-bind
                                ((variable-unbound
                                   (lambda (c)
                                     (let* ((name (name c))
                                            (form (assoc name defconstants)))
                                       ;; no form means the name is undefined,
                                       ;; so just let the error percolate up.
                                       (when form
                                         (parse-defconstant form)
                                         (go try-again))))))
                              (parse-initializer init cenv adt-env))))
                      (setf cenv (make-env (list name) (list init) cenv)))))))
      (loop until (null defconstants)
            do (parse-defconstant (first defconstants)))
      cenv)))

;;; Input is a list of (defvar name [initializer]) forms, and a list of
;;; (defconstant name initializer) forms, and an adt-env.
;;; Returns an alist from variable names to initializers.
(defun parse-definitions (defvars defconstants adt-env)
  (let* ((varnames (mapcar #'second defvars))
         (vars (loop for name in varnames
                     collect (make-instance 'variable :name name)))
         (env (make-env varnames vars))
         ;; Now that we have an environment, figure out constants.
         (cenv (parse-defconstants
                (copy-list defconstants) env adt-env)))
    ;; Now we can simply...
    (loop for (_ name . rest) in defvars
          for init = (if (null rest)
                         (undef)
                         (parse-initializer (first rest) env adt-env))
          collect (cons name init))))
