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
  (assert (null (toplevels pre-module)))
  (with-type-cache ()
    (let* ((varbinds (varbinds pre-module))
           (env (environment pre-module))
           (tmap
             ;; FIXME: Breaking environment abstraction
             (loop for (name . info) in (bindings env)
                   for variable = (variable info)
                   for sc = (or (declared-type info)
                                (let ((tv (make-tvar name)))
                                  (schema tv (list tv))))
                   collect (cons variable sc)))
           (tenv (make-tenv tmap))
           (entries
             (loop for (variable . initializer) in varbinds
                   for inference = (infer-initializer-toplevel initializer
                                                               tenv)
                   collect (make-instance 'entry
                             :variable variable :initializer initializer
                             :inference inference))))
      (make-instance 'module :entries entries))))
