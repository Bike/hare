(in-package #:hare)

#|

There is a static, strong, inference based type system.
Due to memory concerns, types are divided into "sized" and "unsized"
types. Sized types are allocated with WITH, deconstructed with
CASE, and can have pointers to them dereferenced. Unsized types are
allocated with WITH-ARRAY, deconstructed with CASE!, cannot have
pointers to them dereferenced, and cannot be bound to variables or passed
to functions.

New types, called ADTs for Algebraic Data Types, are defined with defadt.
Specifically, defadt defines a type schema, and then you fill it out
with args at some later point.
An ADT can have at most one unsized type argument - kind of like a
flexible array member in C.

Sized types consist of:
 * (int n) is an integer of n bits. Un/signed arithmetic is not part
    of the type, it's part of operations on values, like in LLVM.
 * (pointer x) is a pointer to a value of type x. Pointers are
    sized regardless of whether x is sized.
 * ADTs where all component types are sized.
Unsized types consist of:
 * (function return-type arg-types...) are functions.
 * (array x) is an array of values of sized type x, of indefinite length
    hopefully tracked by other parts of the program. Unlike C, arrays and
    pointers are distinct types and do not interconvert: see AREF
    for how to use them.
 * ADTs where any component type is unsized.

ADTs may (of course) refer to themselves and each other. If this is one way,
there is no problem. However, if there is a recursive dependency, the ADT
is treated as unsized, and furthermore cannot be used directly.

For example, take (defadt foo () (bar (array foo))). This is illegal because
foo is unsized within its own definition. (defadt foo () (bar foo)) is also
illegal, even though having one unsized member would usually be okay.
(defadt foo () (bar (pointer foo))) is okay. (defadt foo (x) (bar (array x))) is okay;
(foo (foo bool)) is not, but that's because an array of arrays isn't valid because
arrays are unsized.

The types byte and word are predefined. They are integer types for
the target architecture's smallest addressable unit and most
conveniently addressable unit, respectively. byte-bits and word-bits
constants are integers with the given n's.

The type bool is predefined as (int 1). Constants true and false are
available; their values as integers are not defined.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cache for types.
;;; It's convenient for equivalent types to be EQ.
;;;

(defclass type-cache ()
  (;; Hash from nonnegative integers to types. Could be a vector...
   (%ints :initform (make-hash-table)
          :reader type-cache-ints)
   ;; Hash from types to types for pointing to them.
   (%pointers :initform (make-hash-table :test #'eq
                                         #+sbcl :weakness #+sbcl :key)
              :reader type-cache-pointers)
   ;; Hash from lists (return ...params) to function types.
   (%funs :initform (make-hash-table :test #'equal)
          :reader type-cache-funs)
   ;; Hash from types to types of arrays of them to them.
   (%arrays :initform (make-hash-table :test #'eq
                                       #+sbcl :weakness #+sbcl :key)
            :reader type-cache-arrays)
   ;; Hash from lists (adt-def ...types) to adts
   (%adts :initform (make-hash-table :test #'equal)
          :reader type-cache-adts)))

(defvar *type-cache*)

(defmacro with-type-cache ((&rest options) &body body)
  (declare (ignore options)) ; for future expansion
  `(let ((*type-cache* (make-instance 'type-cache)))
     ,@body))

(defmacro cached-int (n)
  `(gethash ,n (type-cache-ints *type-cache*)))
(defmacro cached-pointer (ty)
  `(gethash ,ty (type-cache-pointers *type-cache*)))
(defmacro cached-fun (key)
  `(gethash ,key (type-cache-funs *type-cache*)))
(defmacro cached-array (ty)
  `(gethash ,ty (type-cache-arrays *type-cache*)))
(defmacro cached-adt (key)
  `(gethash ,key (type-cache-adts *type-cache*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Types (again, hare style)
;;;

;;; abstract
(defclass type () ())

;;; Given a type, return a sexp representation for display.
(defgeneric unparse-type (type))

;;; Determine is types are equal.
(defun type= (t1 t2)
  ;; Thanks, caching!
  (eq t1 t2))

(defmethod print-object ((o type) s)
  (print-unreadable-object (o s)
    (write 'type :stream s)
    (write-char #\Space s)
    (write (unparse-type o) :stream s)))

;; Apply function to type and its component types. Return NIL.
(defgeneric mapnil-type (function type)
  (:argument-precedence-order type function)
  (:method :before (function (type type)) (funcall function type)))
;; Apply function to type. If it returns non-nil, return that.
;; Otherwise, copy type, recursively map-typing any component types.
;; The type doesn't need to be copied if there are no component types.
(defgeneric map-type (function type)
  (:argument-precedence-order type function)
  (:method :around (function (type type))
    (or (funcall function type) (call-next-method))))

;;; integer type. length is in bits.
;;; (int 1) doubles as bool.
(defclass int (type)
  ((%length :initarg :length :accessor int-type-length
            :type (integer 0))))
(defun make-int (len)
  (or (cached-int len)
      (setf (cached-int len) (make-instance 'int :length len))))
(defun make-bool () (make-int 1))
(defmethod mapnil-type (function (type int)) (declare (ignore function)))
(defmethod map-type (function (type int))
  (declare (ignore function))
  type)
(defmethod unparse-type ((type int))
  `(int ,(int-type-length type)))

;;; pointer.
(defclass pointer (type)
  ((%under :initarg :under :accessor pointer-type-underlying :type type)))
(defun make-pointer (ty)
  (or (cached-pointer ty)
      (setf (cached-pointer ty) (make-instance 'pointer :under ty))))
(defmethod map-type (function (type pointer))
  (make-pointer (map-type function (pointer-type-underlying type))))
(defmethod mapnil-type (function (type pointer))
  (mapnil-type function (pointer-type-underlying type)))
(defmethod unparse-type ((type pointer))
  `(pointer ,(unparse-type (pointer-type-underlying type))))

;;; a function
(defclass fun (type)
  ((%return :initarg :return :accessor fun-return :type type)
   ;; A list of types
   (%params :initarg :params :accessor parameters :type list)))
(defun make-fun (ret params)
  (let ((key (list* ret params)))
    (or (cached-fun key)
        (setf (cached-fun key) (make-instance 'fun
                                 :return ret :params params)))))
(defmethod map-type (function (type fun))
  (make-fun (map-type function (fun-return type))
            (loop for param in (parameters type)
                  collect (map-type function param))))
(defmethod mapnil-type (function (type fun))
  (mapnil-type function (fun-return type))
  (loop for param in (parameters type)
        do (mapnil-type function param)))
(defmethod unparse-type ((type fun))
  `(function ,(unparse-type (fun-return type))
             ,@(mapcar #'unparse-type (parameters type))))

(defclass arrayt (type)
  ((%element-type :initarg :et :accessor arrayt-element-type :type type)))
(defun make-arrayt (ty)
  (or (cached-array ty)
      (setf (cached-array ty) (make-instance 'arrayt :et ty))))
(defmethod map-type (function (type arrayt))
  (make-arrayt (map-type function (arrayt-element-type type))))
(defmethod mapnil-type (function (type arrayt))
  (mapnil-type function (arrayt-element-type type)))
(defmethod unparse-type ((type arrayt))
  `(array ,(unparse-type (arrayt-element-type type))))

;;; Type placeholder used in a few things.
(defclass tvar (type)
  (;; For debugging
   (%name :initarg :name :accessor name :type symbol :initform nil)))
(defun make-tvar (&optional name) (make-instance 'tvar :name name))
(defmethod mapnil-type (function (type tvar)) (declare (ignore function)))
(defmethod map-type (function (type tvar))
  (declare (ignore function))
  type)
(defmethod unparse-type ((type tvar)) (name type))

(defun concrete-type-p (type)
  (mapnil-type (lambda (type)
                 (when (typep type 'tvar)
                   (return-from concrete-type-p nil)))
               type)
  t)

;;; A "subst" is an alist (tvar . type) used to describe substitutions.
(defun empty-subst () nil)

;;; Given a subst and a type, make a new type in which all the
;;; tvars are replaced by the associated type. (Type doesn't have to be new
;;; if there are no changes.)
(defun subst-type (subst type)
  (map-type (lambda (type)
              (when (typep type 'tvar)
                (let ((pair (assoc type subst :test #'eq)))
                  (if pair (cdr pair) nil))))
            type))

(defun compose-subst (subst1 subst2)
  ;; note: diehl uses Map.union, which i believe takes the left
  ;; item if there's a duplicate. CL union leaves this undefined.
  (union (mapcar (lambda (pair)
                   (cons (car pair) (subst-type subst1 (cdr pair))))
                 subst2)
         subst1
         :key #'car))
(defun compose-substs (&rest substs)
  (cond ((null substs) (empty-subst))
        ((null (rest substs)) (first substs))
        (t (compose-subst (first substs)
                          (apply #'compose-substs (rest substs))))))

;;;;;;;;;;;;;;;;;;;;
;;;
;;; ADT stuff
;;;

;;; Definition of an ADT schema thing, as from defadt. Not a type.
(defclass adt-def ()
  ((%name :initarg :name :accessor name :type symbol)
   ;; A list of bound type variables (tvars)
   (%tvars :initarg :tvars :accessor tvars :type list)
   ;; A list of constructor names (symbols)
   (%constructors :initarg :constructors :accessor constructors :type list)
   ;; A list of (type*), corresponding to the constructors.
   (%members :initarg :members :accessor members :type list)))

;;; E.g., if we have (defadt foo (x) ...), and then refer to (foo (int 3))
;;; somewhere, we have one of these, with args = ((int 3)) but it's a type.
(defclass adt (type)
  (;; Which ADT definition this is an instantiation of.
   (%def :initarg :def :accessor adt-def :type adt-def)
   ;; A list of types.
   (%args :initarg :args :accessor adt-args :type list)))
(defun make-adt (def args)
  (let ((key (list* def args)))
    (or (cached-adt key)
        (setf (cached-adt key) (make-instance 'adt :def def :args args)))))
(defmethod map-type (function (type adt))
  (make-adt (adt-def type)
            (loop for ty in (adt-args type)
                  collect (map-type function ty))))
(defmethod mapnil-type (function (type adt))
  (loop for ty in (adt-args type) do (map-type function ty)))
(defmethod unparse-type ((type adt))
  `(,(name (adt-def type)) ,@(mapcar #'unparse-type (adt-args type))))

;;; This is like instantiating a schema, below.
;;; It makes a fresh type variable for each variable bound in the def,
;;; substitutes them into the members, and then returns those.
;;; As a second value it returns the instantiated adt.
;;; That is, it returns a list of lists of types.
(defun instantiate-adt-def (adt-def)
  (let* ((old (tvars adt-def))
         (new (loop for tvar in old collect (make-tvar (name tvar))))
         (map (mapcar #'cons old new)))
    (values
     (flet ((substt (type) (subst-type map type)))
       (loop for member in (members adt-def)
             collect (mapcar #'substt member)))
     (make-adt adt-def new))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type schemata
;;; AKA polytypes. They are NOT types in the above way.
;;; After all, a value can't be a polytype. A polytype must be instantiated.
;;;

(defclass schema ()
  (;; A list of tvars.
   (%tvars :initarg :tvars :accessor tvars :type list)
   ;; The underlying type, in which the tvars are available for use.
   (%type :initarg :type :accessor type :type type)))

;;; Convenience
(defun schema (type &optional tvars)
  (make-instance 'schema :tvars tvars :type type))

(defmethod print-object ((object schema) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "∀~:a: ~a" (mapcar #'name (tvars object))
            (unparse-type (type object)))))

;;; Return a list of tvariables free in type or schema or whatever.
(defun free (type)
  (let ((tvars nil))
    (mapnil-type (lambda (ty)
                   (when (typep ty 'tvar)
                     (pushnew ty tvars :test #'eq)))
                 type)
    tvars))
(defun free-in-schema (schema)
  (set-difference (free (type schema)) (tvars schema) :test #'eq))

;;; Subst
(defun subst-schema (subst schema)
  ;; Remove any part of the subst that refers to a bound variable.
  (let* ((tvars (tvars schema))
         (sans (remove-if (lambda (ty) (member ty tvars :test #'eq))
                          subst
                          :key #'car)))
    (schema (subst-type sans (type schema)) tvars)))

;;; HM operation. Given a schema, return a type with fresh free variables
;;; for all the schema's old bound variables.
(defun instantiate (schema)
  (let ((map (loop for tvar in (tvars schema)
                   collect (cons tvar (make-tvar (name tvar))))))
    (subst-type map (type schema))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsing types.

;;; "Environments" are alists (symbol . type).
;;; ADT environments are as in env.lisp.
(defun parse-type (expr env adt-env)
  (etypecase expr
    (symbol ; must be an alias.
     (let ((pair (assoc expr env)))
       (if pair
           (cdr pair)
           ;; "foo" can be short for "(foo)"
           (make-adt (find-adt-def expr adt-env) nil))))
    (cons
     (cl:case (car expr)
       ((int)
        (destructuring-bind (len) (cdr expr)
          (check-type len (integer 0))
          (make-int len)))
       ((pointer)
        (destructuring-bind (under) (cdr expr)
          (make-pointer (parse-type under env adt-env))))
       ((array)
        (destructuring-bind (et) (cdr expr)
          (make-arrayt (parse-type et env adt-env))))
       ((function)
        (destructuring-bind (ret &rest params) (cdr expr)
          (make-fun (parse-type ret env adt-env)
                    (loop for param in params
                          collect (parse-type param env adt-env)))))
       (otherwise
        (make-adt (find-adt-def (car expr) adt-env)
                  (loop for type in (cdr expr)
                        collect (parse-type type env adt-env))))))))
