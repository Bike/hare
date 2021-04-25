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

(defclass decltype-mixin (info)
  (;; A SCHEMA or NIL, but I don't want to have to depend on type.lisp
   (%declared-type :initarg :type :initform nil :accessor declared-type)))

(defclass variable-info (decltype-mixin info)
  ((%variable :initarg :variable :reader variable)))

(defclass constant-info (decltype-mixin info)
  ((%initializer :initarg :initializer :accessor initializer)))

(defclass macro-info (info)
  ((%expander :initarg :expander :reader expander)))

(defclass symbol-macro-info (decltype-mixin info)
  ((%expander :initarg :expander :reader expander)))

(defclass special-operator-info (info) ())

(defclass environment ()
  ((%parent :accessor parent :initarg :parent :type (or environment null))
   ;; An alist.
   (%bindings :accessor bindings :initarg :bindings :type list)))

(defgeneric parent (environment)) ; an environment or NIL

(defclass global-environment (environment)
  ((%parent :initform nil)))
(defclass lexical-environment (environment)
  ((%parent :type environment)))

;;; Get the global environment from an environment.
(defgeneric global-environment (environment))
(defmethod global-environment ((env global-environment)) env)
(defmethod global-environment ((env lexical-environment))
  (global-environment (parent env)))

;;; Given an environment, return a copy with the specified global environment.
;;; This is used in relation to the lexical environments stored in unparsed
;;; ASTs.
(defgeneric rehome-environment (environment global))
(defmethod rehome-environment ((env global-environment)
                               (global global-environment))
  global)
(defmethod rehome-environment ((env lexical-environment)
                               (global global-environment))
  (make-env (mapcar #'car (bindings env))
            (mapcar #'cdr (bindings env))
            (rehome-environment (parent env) global)))

;;; Given an environment, return a copy with an empty global environment.
;;; This is used for storing lexical environments in unparsed ASTs.
(defun lexical-environment (environment)
  (rehome-environment environment (make-env nil nil)))

(defgeneric lookup (name environment)
  (:argument-precedence-order environment name))

(defmethod lookup (name (env global-environment))
  (or (cdr (assoc name (bindings env) :test #'eq))
      (let ((parent (parent env)))
        (when parent (lookup name parent)))))
(defmethod lookup (name (env lexical-environment))
  (or (cdr (assoc name (bindings env) :test #'eq)) (lookup name (parent env))))

(defgeneric (setf lookup) (new name environment)
  (:argument-precedence-order environment name new))

(defmethod (setf lookup) (new name (env global-environment))
  (let ((pair (assoc name (bindings env) :test #'eq)))
    (if pair
        (setf (cdr pair) new)
        (push (cons name new) (bindings env))))
  new)

(defun make-env (names values &optional parent)
  (if parent
      (make-instance 'lexical-environment
        :bindings (mapcar #'cons names values) :parent parent)
      (make-instance 'global-environment
        :bindings (mapcar #'cons names values))))

(defun make-global-env (names values &optional parent)
  (make-instance 'global-environment
    :bindings (mapcar #'cons names values) :parent parent))

(defun map-env (function env)
  (loop for (name . info) in (bindings env) do (funcall function name info)))
