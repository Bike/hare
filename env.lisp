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
