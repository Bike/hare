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
             :accessor aliases :type list)))
(defclass global-type-env (type-env)
  (;; ADTs
   (%by-name :initform (make-hash-table :test #'eq) :initarg :by-name
             :accessor by-name :type hash-table)
   (%by-constructor :initform (make-hash-table :test #'eq)
                    :initarg :by-constructor
                    :accessor by-constructor :type hash-table)))
(defclass lexical-type-env (type-env)
  ((%parent :initarg :parent :type type-env)))
(defgeneric global-type-env (type-env))
(defmethod global-type-env ((te global-type-env)) te)
(defmethod global-type-env ((te lexical-type-env))
  (global-type-env (parent te)))
(defgeneric rehome-type-env (type-env global))
(defmethod rehome-type-env ((te global-type-env) (g global-type-env)) g)
(defmethod rehome-type-env ((te lexical-type-env) (g global-type-env))
  (make-instance 'lexical-type-env :aliases (aliases te)
                 :parent (rehome-type-env (parent te) g)))
(defun lexical-type-env (type-env) (rehome-type-env type-env (make-type-env)))

(defgeneric find-alias (name type-env)
  (:argument-precedence-order type-env name))

(defmethod find-alias (name (te global-type-env))
  (cdr (assoc name (aliases te))))
(defmethod find-alias (name (te lexical-type-env))
  (or (cdr (assoc name (aliases te)))
      (find-alias name (parent te))))

(defun add-alias (name params expansion type-env)
  (push (list name params expansion) (aliases type-env)))

(defun find-adt-def (name type-env)
  (gethash name (by-name (global-type-env type-env))))

(defun find-constructor (constructor-name type-env)
  (gethash constructor-name (by-constructor (global-type-env type-env))))

(defun make-type-env () (make-instance 'global-type-env))

;; Return a new type-env with additional name->type mapping.
(defun augment-type-env (type-env aliases)
  (make-instance 'lexical-type-env
    :parent type-env :aliases aliases))

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
