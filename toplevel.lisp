(in-package #:hare)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interpreting toplevel forms
;;; Given a list of toplevel forms, we process, in order:
;;; 1) deftype
;;; 2) defadt
;;; 3) defun, defvar
;;; This isn't part of the language definition, except in that defadt
;;;  never depends on a defun, etc.
;;;

(defclass toplevel () ())

(defclass toplevel-defun (toplevel)
  ((%name :accessor name :initarg :name :type symbol)
   ;; list of locals
   ;; FIXME: Should allow ad hoc polymorphism yada yada
   (%params :accessor params :initarg :params :type list)
   (%body :accessor body :initarg :body :type ast)))

(defun parse-defun (defun adt-env)
  (destructuring-bind (defun name params . body) defun
    (declare (ignore defun))
    (let ((locals (loop for param in params
                        collecting (make-instance 'local :name name))))
      (make-instance 'toplevel-defun
                     :name name :params locals
                     :body (parse-form `(seq ,@body)
                                       (mapcar #'cons params locals)
                                       adt-env)))))

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

;;; Input is a bunch of (defadt name (tvar*) (constructor type*)*) forms
(defun parse-defadts (forms)
  (loop for form in forms
        for name = (second form)
        for vars = (third form)
        for tvars = (loop for var in vars
                          collect (make-tvar var))
        for def = (make-instance 'adt-def :name name :tvars tvars)
        collect (cons def form) into todo
        collect (cons name def) into adt-env
        finally (loop for (def . form) in todo
                      do (parse-defadt def form adt-env))
                (return adt-env)))

#+(or)
(defun process-toplevels (toplevels)
  ;; dut dut not actually doing anything but defuns yet
  (let (deftypes defadts defuns)
    (loop for toplevel in toplevels
          do (ecase (car toplevel)
               #+(or)((deftype) (push toplevel deftypes))
               #+(or)((defadt) (push toplevel defadts))
               ((defun)
                (push (parse-defun toplevel nil) defuns))))
    (let* (#|process defadts and deftypes here...|#
           ;; Our initial type environment is simple:
           ;; every name is bound to a polymorphic pointer.
           ;; Hypothetically we could bind functions specifically to functions,
           ;; but strictly speaking it shouldn't be necessary...
           (fun-tvars (loop for defun in defuns
                            for name = (name defun)
                            collect (make-tvar name)))
           (fun-types (mapcar #'make-pointer fun-tvars))
           (tenv (loop for defun in defuns
                       for fun-tvar in fun-tvars
                       for fun-type in fun-types
                       for name = (name defun)
                       collect (cons name (schema fun-type (list fun-tvar))))))
      ;; Loop through asts and inference them.
      ;; When we get a type for a function's body,
      ;; unify it with (function ret-type paramtypes...)
      ;; and apply that substitution to the whole env.
      (loop for defun in defuns
            for fun-type in fun-types
            for params = (params defun) ; list of locals, remember
            for tvars = (loop for param in params
                              collecting (make-tvar (name param)))
            for schemata = (mapcar #'schema tvars)
            for local-tenv = (extend-tenv-list params schemata tenv)
            ;; Now infer
            do (multiple-value-bind (ret-type subst)
                   (infer (body defun) local-tenv)
                 (let* ((new-paramtypes
                          (loop for tvar in tvars
                                collect (subst-type subst tvar)))
                        (ftype (make-fun ret-type new-paramtypes))
                        (funsubst (unify fun-type ftype))
                        (subst (compose-subst fun-subst subst)))
                   ;; Update the tenv!
                   (setf tenv (subst-tenv subst tenv)))))
      ;; Now we have defuns with typed bodies.
      ;; All we have to do now is dump them out. Ha. Ha.
      )))
