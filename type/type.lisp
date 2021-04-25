(in-package #:hare.type)

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
(defadt foo () (bar (pointer foo))) is okay. (defadt foo (x) (bar (array x)))
is okay; (foo (foo bool)) is not, but that's because an array of arrays isn't
valid because arrays are unsized.

The types byte and word are predefined. They are integer types for
the target architecture's smallest addressable unit and most
conveniently addressable unit, respectively. byte-bits and word-bits
constants are integers with the given n's.
|#

;;; abstract
(defclass type () ())

;;; Given a type, return a sexp representation for display.
(defgeneric unparse-type (type))

(defgeneric type= (type1 type2)
  (:method ((t1 type) (t2 type)) nil) ; default
  (:method :around ((t1 type) (t2 type)) ; optimization
    (or (eq t1 t2) (call-next-method))))

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
(defclass int (type)
  ((%length :initarg :length :accessor int-type-length
            :type (integer 0))))
(defun make-int (len)
  (or (cached-int len)
      (setf (cached-int len) (make-instance 'int :length len))))
(defmethod type= ((t1 int) (t2 int))
  (= (int-type-length t1) (int-type-length t2)))
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
(defmethod type= ((t1 pointer) (t2 pointer))
  (type= (pointer-type-underlying t1) (pointer-type-underlying t2)))
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
(defmethod type= ((t1 fun) (t2 fun))
  (and (type= (fun-return t1) (fun-return t2))
       (= (length (parameters t1)) (length (parameters t2)))
       (every #'type= (parameters t1) (parameters t2))))
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
(defmethod type= ((t1 arrayt) (t2 arrayt))
  (type= (arrayt-element-type t1) (arrayt-element-type t2)))
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

;;; Pseudo-type used when parsing failed.
(defclass unknown (type)
  ((%form :initarg :form :accessor form)))

;;; For algebraic data types, see adt.lisp. They're a little more complicated.
