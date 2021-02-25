(in-package #:hare)

#|

During compilation we make use of two kinds of environments, "environments" and
"type environments". The former hold bindings from names in the "variable
namespace" I guess I'll call it to either variable or initializer objects.

The function LOOKUP gets things from the first kind of environment.
MAKE-ENV can be used to make a new environment. It takes a list of names and
a list of values (i.e. variables or initializers), and optionally a parent env.

|#

;;; The class of things stored in these environments.
(defclass info () ())

(defclass variable-info (info)
  ((%variable :initarg :variable :reader variable)))

(defclass constant-info (info)
  ((%initializer :initarg :initializer :accessor initializer)))

(defclass macro-info (info)
  ((%expander :initarg :expander :reader expander)))

(defclass symbol-macro-info (info)
  ((%expander :initarg :expander :reader expander)))

(defclass special-operator-info (info) ())

(defclass environment ()
  (;; An alist.
   (%bindings :accessor bindings :initarg :bindings :type list)))

(defun lookup (name environment)
  (let ((pair (assoc name (bindings environment) :test #'eq)))
    (if pair
        (cdr pair)
        (error 'variable-unbound :name name))))

(defun forgiving-lookup (name environment)
  (cdr (assoc name (bindings environment) :test #'eq)))

(defun (setf lookup) (new name environment)
  (let ((pair (assoc name (bindings environment) :test #'eq)))
    (if pair
        (setf (cdr pair) new)
        (push (cons name new) (bindings environment))))
  new)

(defun make-env (variables values &optional parent)
  (let ((parent-bindings (if parent (bindings parent) nil)))
    (make-instance 'environment
      :bindings (nconc (mapcar #'cons variables values) parent-bindings))))
