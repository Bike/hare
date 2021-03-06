(in-package #:hare.ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Literals and initializers
;;;
;;; A "literal" is a source representation of an object that can exist at
;;; runtime as bound to a variable. An "initializer" is a source representation
;;; of the initial value stored in a pointer.
;;; All literals are initializers, but some initializers are not literals.
;;;
;;; Literals consist of:
;;;  * Integer constants (all positive and decimal, for the moment).
;;;  * Names defined with defconstant.
;;;  * (constructor literal*) where constructor is from some ADT and sized.
;;;
;;; Initializers consist of:
;;;  * Integer constants.
;;;  * Names defined with defconstant.
;;;  * Names defined with defvar (pointers).
;;;  * (constructor initializer*) where constructor is from some ADT and may
;;;    or may not be sized.
;;;  * undef
;;;  * (array initializer*) where the initializers are of uniform type.
;;;  * (arrayn n) where n is an integer constant. Short for an array of length
;;;    n where all elements are undef. This will be defined as a macro.
;;;  * (lambda (symbol*) form*) is a function.
;;;  * (vla form) is an array with a length defined at runtime by the form,
;;;    which must evaluate to an integer. All elements are undef.
;;;    This initializer is only permitted within a function, i.e. is not
;;;    allowed at top level.
;;;
;;; Literals and initializers may be polymorphically typed.
;;; Integer constants may be instantiated as any integer type that can fit
;;; the value.
;;;
;;; undef indicates to the compiler that this value will not be read before
;;; it is initialized, and that doing so is undefined behavior. This allows
;;; things to be only partially initialized if that's convenient.
;;;
;;; Because I don't think it's possible to indicate in LLVM, lambda initializers
;;; are currently only valid at top level. FIXME.
;;;
;;; NOTE: The array (not arrayn), bytes, and lambda initializer types kind
;;; of imply some accessible storage when used other than a defvar.
;;; That is, imagine we have (with (x (array 10 12 13)) ...)
;;; At runtime, we have to allocate an array, and then fill it. The fill data
;;; would probably have to be stored somewhere else, so that in effect we
;;; reduce this to (with-array (x 3) (memcpy x array_10_12_13 3) ...). The
;;; memcpy doesn't actually have to be a memcpy - since the length is known it
;;; could just copy in the bytes individually, and that would obviate the need
;;; for extra storage - but I don't know, it's still a little bit weird.
;;; SUB-NOTE: An array of undefs is fine, obviously.
;;;
;;; TODO?: For some things it's useful to express a pointer by its numerical
;;; address.
;;;
;;; We use the same classes for initializers and literals.
;;;

;;; abstract
(defclass initializer ()
  ((%type :accessor type :initarg :type :type type:type)))

(defclass integer-initializer (initializer)
  ((%value :accessor value :initarg :value :type (cl:integer 0))))

(defmethod print-object ((i integer-initializer) stream)
  (print-unreadable-object (i stream :type t)
    (write (value i) :stream stream)))

;;; An initializer that is a toplevel variable.
(defclass variable-initializer (initializer)
  ((%variable :accessor variable :initarg :variable :type variable)))

(defmethod print-object ((i variable-initializer) stream)
  (print-unreadable-object (i stream :type t)
    (write (name (variable i)) :stream stream)))

(defclass constructor-initializer (initializer)
  ((%constructor :accessor constructor :initarg :constructor
                 :type constructor)
   ;; A list of initializers.
   (%fields :accessor fields :initarg :fields :type list)))

(defclass undef-initializer (initializer) ())

(defun undef () (make-instance 'undef-initializer))

(defclass lambda-initializer (initializer)
  (;; A list of variables.
   (%params :accessor params :initarg :params :type list)
   (%body :accessor body :initarg :body :type ast)))

(defmethod print-object ((i lambda-initializer) stream)
  (print-unreadable-object (i stream :type t)
    (write (mapcar #'name (params i)) :stream stream)))

(defclass array-initializer (initializer)
  (;; A list of initializers.
   (%elements :accessor elements :initarg :elements :type list)))

(defclass vla-initializer (initializer)
  ((%nelements :accessor nelements :initarg :nelements :type ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mapping
;;;

(defgeneric mapnil-initializer (function initializer)
  (:argument-precedence-order initializer function)
  (:method :before (function (initializer initializer))
    (funcall function initializer)))
(defgeneric map-initializer (function initializer)
  (:argument-precedence-order initializer function)
  (:method :around (function (i initializer))
    (or (funcall function i) (call-next-method))))
(defun copy-initializer (initializer)
  (map-initializer (constantly nil) initializer))

(defmethod mapnil-initializer (function (i integer-initializer))
  (declare (ignore function)))
(defmethod map-initializer (function (i integer-initializer))
  (make-instance 'integer-initializer :value (value i) :type (type i)))

(defmethod mapnil-initializer (function (i variable-initializer))
  (declare (ignore function)))
(defmethod map-initializer (function (i variable-initializer))
  (make-instance 'variable-initializer :variable (variable i) :type (type i)))

(defmethod mapnil-initializer (function (i constructor-initializer))
  (loop for field in (fields i)
        do (mapnil-initializer function field)))
(defmethod map-initializer (function (i constructor-initializer))
  (make-instance 'constructor-initializer
    :constructor (constructor i) :type (type i)
    :fields (loop for field in (fields i)
                  collect (map-initializer function field))))

(defmethod mapnil-initializer (function (i undef-initializer))
  (declare (ignore function)))
(defmethod map-initializer (function (i undef-initializer))
  (make-instance 'undef-initializer :type (type i)))

(defmethod mapnil-initializer (function (i lambda-initializer))
  (mapnil-ast function (body i)))
(defmethod map-initializer (function (i lambda-initializer))
  (make-instance 'lambda-initializer
    :params (params i) :type (type i)
    :body (map-ast function (body i))))

(defmethod mapnil-initializer (function (i array-initializer))
  (loop for elem in (elements i)
        do (mapnil-initializer function elem)))
(defmethod map-initializer (function (i array-initializer))
  (make-instance 'array-initializer
    :type (type i) :elements (loop for e in (elements i)
                                   collect (map-initializer function e))))

(defmethod mapnil-initializer (function (i vla-initializer))
  (mapnil-ast (nelements i)))
(defmethod map-initializer (function (i vla-initializer))
  (make-instance 'vla-initializer
    :type (type i) :nelements (map-ast function (nelements i))))
