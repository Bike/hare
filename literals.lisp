(in-package #:hare)

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
;;;  * (constructor literal*) where constructor is from some ADT and must be
;;;    sized.
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
;;;    n where all elements are undef.
;;;  * (lambda (symbol*) form*) is a function.
;;;  * (bytes initializer) indicates a byte vector where the bytes are taken
;;;    from the object representation corresponding to the initializer.
;;;
;;; Literals and initializers may be polymorphically typed.
;;; Integer constants may be instantiated as any integer type that can fit
;;; the value.
;;;
;;; undef indicates to the compiler that this value will not be read before
;;; it is initialized, and that doing so is undefined behavior. This allows
;;; things to be only partially initialized if that's convenient.
;;;
;;; Because I don't think it's possible to indicate in C, lambda initializers
;;; are currently only valid at top level. FIXME.
;;; Array initializers, including bytes, are not implemented. FIXME.
;;; Constant references not implemented because circularity and early
;;; definitions need to be dealt with. FIXME.
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
(defclass initializer () ())

(defclass integer-initializer (initializer)
  ((%value :accessor value :initarg :value :type (integer 0))))

(defclass constructor-initializer (initializer)
  ((%def :accessor adt-def :initarg :def :type adt-def)
   (%constructor :accessor constructor :initarg :constructor
                 :type symbol)
   ;; A list of initializers.
   (%fields :accessor fields :initarg :fields :type list)))

(defclass undef-initializer (initializer) ())

(defclass lambda-initializer (initializer)
  (;; A list of variables.
   (%params :accessor params :initarg :params :type list)
   (%body :accessor body :initarg :body :type ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsing
;;;

(defun parse-literal (literal constant-env adt-env)
  (etypecase literal
    ((integer 0) (make-instance 'integer-initializer :value literal))
    ((or (cons (member array arrayn bytes lambda)) (eql undef))
     (error "Found initializer in literal context: ~a" literal))
    (symbol (error "Constants not implemented yet")
     #+(or)(find-constant literal constant-env))
    (cons ; constructor
     (let* ((constructor (car literal)) (fields (cdr literal))
            (def (find-adt-def constructor adt-env)))
       (make-instance 'constructor-initializer
         :def def :constructor constructor
         :fields (loop for field in fields
                       collect (parse-literal field
                                              constant-env adt-env)))))))

(defun parse-initializer (initializer constant-env adt-env)
  (etypecase initializer
    ((integer 0) (make-instance 'integer-initializer :value initializer))
    ((eql undef) (make-instance 'undef-initializer))
    (symbol (error "Constants not implemented yet")
     #+(or)(find-constant initializer constant-env))
    ((cons (eql lambda))
     (parse-lambda (cadr initializer) (cddr initializer)
                   constant-env adt-env))
    ((cons (member array arrayn bytes))
     (error "Not implemented yet: ~a" (car initializer)))
    (cons ; constructor
     (let* ((constructor (car initializer)) (fields (cdr initializer))
            (def (find-adt-def constructor adt-env)))
       (make-instance 'constructor-initializer
         :def def :constructor constructor
         :fields (loop for field in fields
                       collect (parse-initializer
                                field constant-env adt-env)))))))

(defun parse-lambda (params forms constant-env adt-env)
  (let* ((vars (mapcar #'make-variable params))
         (env (make-env params vars)))
    (make-instance 'lambda-initializer
      :params vars
      :body (parse-form `(seq ,@forms) env constant-env adt-env))))
