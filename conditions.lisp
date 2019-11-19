(in-package #:hare)

(define-condition variable-unbound (error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "The variable ~a is unbound."
                     (name condition)))))
