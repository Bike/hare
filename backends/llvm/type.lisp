(in-package #:hare-llvm)

(defgeneric type->llvm (type))

(defmethod type->llvm ((type type:int))
  (llvm:int-type (type:int-type-length type)))

(defmethod type->llvm ((type type:pointer))
  (llvm:pointer-type
   (type->llvm (type:pointer-type-underlying type))))

(defmethod type->llvm ((type type:fun))
  (llvm:function-type
   (type->llvm (type:fun-return type))
   (mapcar #'type->llvm (type:parameters type))))

(defmethod type->llvm ((type type:arrayt))
  (llvm:array-type (type->llvm (type:arrayt-element-type type)) 0))

;;; Sum types (ADTs with more than one constructor) are tricky to describe in
;;; LLVM, because it has no direct representation of them, and more annoyingly,
;;; does not permit bitcasting aggregate (structure) types.
;;; For an ADT in memory we can skip this by having structures consisting of a
;;; tag and an indefinitely sized array of i8, but for structures we can
;;; directly deal with (i.e. not through pointers) this is not possible, as
;;; arrays are not first class. What we need to do is come up with a kind of
;;; least common denominator that can work for any of the constructors.
;;; As a KLUDGEy first approximation, we can do an i64 for each field, and cast
;;; these i64s wildly.

;;; A "layout" represents how an object is actually laid out in memory.
;;; In the future the ability to specify the layout of objects will be somewhat
;;; exported to the user (e.g. to specify ABIs, deal with hardware, optimize)
;;; but for now it's internal.
(defclass layout ()
  (;; An LLVM type
   (%ltype :initarg :ltype :reader ltype)))

(defclass adt-layout (layout) ())

;;; A "direct" translation as an LLVM struct.
(defclass direct-layout (adt-layout) ())

(defun compute-direct-layout (adt)
  (let* ((constructor (first (type:constructors adt)))
         (struct (llvm:struct-create-named
                  ;; TODO: Put the parameter types in the name.
                  (string-downcase (symbol-name (type:name constructor))))))
    (llvm:struct-set-body struct
                          (mapcar #'type->llvm (type:fields constructor)))
    (make-instance 'direct-layout :ltype struct)))

;;; A dumb translation as a struct of a sequence of i64s.
(defclass dumb-layout (adt-layout) ())

;;; How many words does this type need to be represented?
;;; This function is enormously KLUDGEy. It would be more correct to get info
;;; about sizes from LLVM, but it seems to be rather involved to do so. You
;;; want to use the DataLayout class, except that it does not seem to be very
;;; well exposed to the C API.
(defgeneric nwords (type))
(defmethod nwords ((ty type:int))
  (if (> (type:int-type-length ty) 64)
      (error "BUG: Not implemented yet")
      1))
(defmethod nwords ((ty type:pointer)) 1)

(defmethod nwords ((ty type:adt))
  (1+ ; tag
   (loop for constructor in (type:constructors ty)
         maximize (loop for field in (type:fields constructor)
                        sum (nwords field)))))

(defun compute-dumb-layout (adt)
  (let* ((def (type:adt-def adt))
         (name (string-downcase (symbol-name (type:name def))))
         (struct (llvm:struct-create-named name))
         (nwords (nwords adt)))
    (llvm:struct-set-body struct
                          (loop repeat nwords collect (llvm:int64-type)))
    (make-instance 'dumb-layout :ltype struct)))

(defun direct-layout-adt-p (adt)
  (= (length (type:constructors (type:adt-def adt))) 1))

(defun compute-layout (adt)
  (if (direct-layout-adt-p adt)
      (compute-direct-layout adt)
      (compute-dumb-layout adt)))

(defvar *types*)

(defun layout (adt)
  (or (values (gethash adt *types*))
      (setf (gethash adt *types*) (compute-layout adt))))

(defmethod type->llvm ((type type:adt)) (ltype (layout type)))
