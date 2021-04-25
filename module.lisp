(in-package #:hare)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modules
;;; As described in the README, phase 1 modules contain polytyped templates.

(defclass module ()
  (;; A proper list of entries
   (%entries :initarg :entries :reader entries :type list)))

(defclass entry ()
  ((%variable :initarg :variable :reader variable :type variable)
   (%initializer :initarg :initializer :reader initializer :type initializer)
   (%inference :initarg :inference :reader inference :type inference)))

(defun module (pre-module)
  (assert (completep pre-module))
  (type:with-type-cache ()
    (let* ((varbinds (varbinds pre-module))
           (env (environment pre-module))
           (tmap
             (let ((r nil))
               (map-env (lambda (name info)
                          (when (typep info 'variable-info)
                            (let ((variable (variable info))
                                  (sc (or (declared-type info)
                                          (let ((tv (type:make-tvar name)))
                                            (type:schema (type:make-pointer tv)
                                                         (list tv))))))
                              (push (cons variable sc) r))))
                        env)
               r))
           (tenv (make-tenv tmap))
           (entries
             (loop for (variable . initializer) in varbinds
                   for inference = (infer-initializer-toplevel initializer
                                                               tenv)
                   collect (make-instance 'entry
                             :variable variable :initializer initializer
                             :inference inference))))
      (make-instance 'module :entries entries))))
