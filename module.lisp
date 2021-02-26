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
  (let* ((varbinds (varbinds pre-module))
         (tmap
           (loop for (variable) in varbinds
                 ;; FIXME: We're straight up ignoring type declamations here.
                 for tv = (make-tvar (name variable))
                 for sc = (schema tv (list tv))
                 collect (cons variable sc)))
         (tenv (make-tenv tmap))
         (entries
           (loop for (variable . initializer) in varbinds
                 for inference = (infer-initializer-toplevel initializer
                                                             tenv))))
    (make-instance 'module :entries entries)))
