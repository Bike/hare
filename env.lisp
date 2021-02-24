(in-package #:hare)

#|

During compilation we make use of two kinds of environments, "environments" and
"type environments". The former hold bindings from names in the "variable
namespace" I guess I'll call it to either variable or initializer objects. The
latter hold bindings from names in the ADT namespace and constructor namespace
to adt-defs, and names in the type namespace to types.

The function LOOKUP gets things from the first kind of environment.
MAKE-ENV can be used to make a new environment. It takes a list of names and
a list of values (i.e. variables or initializers), and optionally a parent env.

For type envs, the functions FIND-ADT-DEF and FIND-ADT-DEF-FROM-CONSTRUCTOR are
available, as well as FIND-TYPE. MAKE-TYPE-ENV makes an empty environment and ADD-ADT-DEF and ADD-TYPE modify one.

|#

(defclass variable ()
  ((%name :accessor name :initarg :name :type symbol)))
(defun make-variable (name)
  (check-type name symbol)
  (make-instance 'variable :name name))

(defmethod print-object ((o variable) s)
  (print-unreadable-object (o s :type t)
    (write (name o) :stream s)))

;;; initializers are defined in literals.lisp

(defclass environment ()
  (;; An alist.
   (%bindings :accessor bindings :initarg :bindings :type list)))

(defun lookup (name environment)
  (let ((pair (assoc name (bindings environment) :test #'eq)))
    (if pair
        (cdr pair)
        (error 'variable-unbound :name name))))

(defun make-env (variables values &optional parent)
  (let ((parent-bindings (if parent (bindings parent) nil)))
    (make-instance 'environment
      :bindings (nconc (mapcar #'cons variables values) parent-bindings))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; type envs
;;;

(defclass type-env ()
  (;; Type aliases. Alist (name . type)
   (%types :initform (make-hash-table :test #'eq) :accessor types
           :type list)
   ;; ADTs
   (%by-name :initform (make-hash-table :test #'eq) :accessor by-name
             :type hash-table)
   (%by-constructor :initform (make-hash-table :test #'eq)
                    :accessor by-constructor :type hash-table)))

(defun find-type (name type-env)
  (values (cdr (assoc name (types type-env)))))

(defun find-adt-def (name type-env)
  (or (gethash name (by-name type-env))
      (error "Unknown ADT: ~a" name)))

(defun find-adt-def-from-constructor (constructor type-env)
  (or (gethash constructor (by-constructor type-env))
      (error "Unknown constructor: ~a" constructor)))

(defun make-type-env () (make-instance 'type-env))

;; Return a new type-env with additional name->type mapping.
(defun augment-type-env (type-env names types)
  (make-instance 'type-env
    :by-name (by-name type-env) :by-constructor (by-constructor type-env)
    :types (append (mapcar #'cons names types) (types type-env))))

;; Add an adt-def that may only have its name to the environment.
(defun add-adt-def (adt-def type-env)
  (let ((name (name adt-def)) (by-name (by-name type-env)))
    (if (nth-value 1 (gethash name by-name))
        (error "ADT multiply defined: ~a" name)
        (setf (gethash name by-name) adt-def))))

;;; Finish adding a now-complete adt-def to the environment.
(defun finish-adt-def (adt-def type-env)
  (loop with by-constructor = (by-constructor type-env)
        for constructor in (constructors adt-def)
        do (if (nth-value 1 (gethash constructor by-constructor))
               (error "Constructor multiply defined: ~a" constructor)
               (setf (gethash constructor by-constructor) adt-def)))
  (values))
