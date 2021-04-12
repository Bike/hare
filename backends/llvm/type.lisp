(in-package #:hare-llvm)

(defgeneric type->llvm (type))

(defmethod type->llvm ((type hare:int))
  (llvm:int-type (hare:int-type-length type)))

(defmethod type->llvm ((type hare:pointer))
  (llvm:pointer-type
   (type->llvm (hare:pointer-type-underlying type))))

(defmethod type->llvm ((type hare:fun))
  (llvm:function-type
   (type->llvm (hare:fun-return type))
   (mapcar #'type->llvm (hare:parameters type))))

;;; So this is pretty weird, right? Yes. Here's the skivvy. In C, arrays and
;;; pointers are closely identified; array expressions are converted into
;;; pointers in almost all contexts. LLVM is somewhat more explicit, so we can
;;; do things like declare functions have pointers to arrays as arguments, but
;;; these are distinct from regular pointers. So if HYPOTHETICALLY we wanted to
;;; describe C puts, say, we say it takes an i8*. But in Hare terms a C string
;;; is a pointer to an i8 array, which is a different type, so we can't pass
;;; our strings to puts so well.
;;; Point is, defining this method like this means that (pointer (array i8))
;;; will end up as i8* like in C.
;;; Now that I'm coming down from the exuberance of finally fucking printing
;;; anything, I think I need to pronounce this method a KLUDGE. First off it
;;; means we need to special case it whenever we actually do mean an array type,
;;; like in constants or struct definitions. Secondly, once array operators are
;;; defined, there will be an operator ("aref", probably) to get a pointer to
;;; element from a pointer to array, i.e. "&(arr[n])" in C. Then if we want to
;;; call C puts or whatnot we just use that operator, which of course compiles
;;; down to at most a LEA.
;;; I don't actually know if LLVM allows pointer to array as a function
;;; parameter type. Guess I will find out!
(defmethod type->llvm ((type hare:arrayt))
  (type->llvm (hare:arrayt-element-type type)))

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
  (let* ((constructor (first (hare::constructors adt)))
         (struct (llvm:struct-create-named
                  ;; TODO: Put the parameter types in the name.
                  (string-downcase (symbol-name (hare:name constructor))))))
    (llvm:struct-set-body struct
                          (mapcar #'type->llvm (hare::fields constructor)))
    (make-instance 'direct-layout :ltype struct)))

;;; A dumb translation as a struct of a sequence of i64s.
(defclass dumb-layout (adt-layout) ())

;;; How many words does this type need to be represented?
;;; This function is enormously KLUDGEy. It would be more correct to get info
;;; about sizes from LLVM, but it seems to be rather involved to do so. You
;;; want to use the DataLayout class, except that it does not seem to be very
;;; well exposed to the C API.
(defgeneric nwords (type))
(defmethod nwords ((ty hare:int))
  (if (> (hare:int-type-length ty) 64)
      (error "BUG: Not implemented yet")
      1))
(defmethod nwords ((ty hare:pointer)) 1)

(defmethod nwords ((ty hare:adt))
  (1+ ; tag
   (loop for constructor in (hare::constructors ty)
         maximize (loop for field in (hare::fields constructor)
                        sum (nwords field)))))

(defun compute-dumb-layout (adt)
  (let* ((def (hare:adt-def adt))
         (name (string-downcase (symbol-name (hare:name def))))
         (struct (llvm:struct-create-named name))
         (nwords (nwords adt)))
    (llvm:struct-set-body struct
                          (loop repeat nwords collect (llvm:int64-type)))
    (make-instance 'dumb-layout :ltype struct)))

(defun direct-layout-adt-p (adt)
  (= (length (hare:constructors (hare:adt-def adt))) 1))

(defun compute-layout (adt)
  (if (direct-layout-adt-p adt)
      (compute-direct-layout adt)
      (compute-dumb-layout adt)))

(defvar *types*)

(defun layout (adt)
  (or (values (gethash adt *types*))
      (setf (gethash adt *types*) (compute-layout adt))))

(defmethod type->llvm ((type hare:adt)) (ltype (layout type)))
