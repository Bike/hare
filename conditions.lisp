(in-package #:hare)

(define-condition variable-unbound (error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "The variable ~a is unbound."
                     (name condition)))))

(define-condition unknown-adt (error)
  ((%name :initarg :name :reader name :type symbol))
  (:report (lambda (condition stream)
             (format stream "Unknown ADT: ~a" (name condition)))))

(define-condition unknown-constructor (error)
  ((%name :initarg :name :reader name :type symbol))
  (:report (lambda (condition stream)
             (format stream "Unknown constructor: ~a" (name condition)))))
