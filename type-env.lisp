(in-package #:hare)

#|
These have bindings from names in the ADT namespace and constructor namespace
to adt-defs, and alias mappings.

The functions FIND-ADT-DEF and FIND-ADT-DEF-FROM-CONSTRUCTOR are available,
as well as FIND-TYPE. MAKE-TYPE-ENV makes an empty environment and ADD-ADT-DEF
and ADD-TYPE modify one.
|#

(defclass type-env ()
  (;; Type aliases. List of (name (tvar*) type)
   (%aliases :initform nil :initarg :aliases
             :accessor aliases :type list)
   ;; ADTs
   (%by-name :initform (make-hash-table :test #'eq) :initarg :by-name
             :accessor by-name :type hash-table)
   (%by-constructor :initform (make-hash-table :test #'eq)
                    :initarg :by-constructor
                    :accessor by-constructor :type hash-table)))

(defun find-alias (name type-env)
  (values (cdr (assoc name (aliases type-env)))))

(defun add-alias (name params expansion type-env)
  (push (list name params expansion) type-env))

(defun find-adt-def (name type-env)
  (or (gethash name (by-name type-env)) (error 'unknown-adt :name name)))

(defun find-constructor (constructor-name type-env)
  (or (gethash constructor-name (by-constructor type-env))
      (error 'unknown-constructor :name constructor-name)))

(defun make-type-env () (make-instance 'type-env))

;; Return a new type-env with additional name->type mapping.
(defun augment-type-env (type-env aliases)
  (make-instance 'type-env
    :by-name (by-name type-env) :by-constructor (by-constructor type-env)
    :aliases (append aliases (aliases type-env))))

;; Add an adt-def that may only have its name to the environment.
(defun add-adt-def (adt-def type-env)
  (let ((name (name adt-def)) (by-name (by-name type-env)))
    (if (nth-value 1 (gethash name by-name))
        (error "ADT multiply defined: ~a" name)
        (setf (gethash name by-name) adt-def))))

(defun add-adt-constructor (constructor type-env)
  (let ((by-constructor (by-constructor type-env)))
    (if (nth-value 1 (gethash constructor by-constructor))
        (error "Constructor multiply defined: ~a" constructor)
        (setf (gethash (name constructor) by-constructor) constructor))))
