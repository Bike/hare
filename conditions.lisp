(in-package #:hare)

(define-condition unknown (error)
  ((%name :initarg :name :reader name)
   (%placeholder :initarg :placeholder :reader placeholder)))

(define-condition unknown-variable (unknown) ())
(define-condition unknown-constructor (unknown) ())
(define-condition unknown-type (unknown) ())
