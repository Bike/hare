(in-package #:hare-llvm)

(defgeneric type->llvm (type))

(defmethod type->llvm ((type int))
  (llvm:int-type (int-type-length type)))

(defmethod type->llvm ((type pointer))
  (llvm:pointer-type
   (type->llvm (pointer-type-underlying type))))

(defmethod type->llvm ((type fun))
  (llvm:function-type
   (type->llvm (fun-return type))
   (mapcar #'type->llvm (parameters type))))

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
(defmethod type->llvm ((type arrayt))
  (type->llvm (arrayt-element-type type)))

(defun constructor->llvm (constructor map)
  (let ((struct (llvm:struct-create-named
                 ;; TODO: Put the parameter types in the name.
                 (string-downcase (symbol-name (hare:name constructor))))))
    (llvm:struct-set-body struct
                          (loop for field in (hare::fields constructor)
                                collect (type->llvm
                                         (hare::subst-type map field))))
    struct))

;; We want multiple type->llvm calls on the same struct type to return the same
;; LLVM type.
(defvar *types*)

(defmethod type->llvm :around ((type adt))
  (or (values (gethash type *types*))
      (setf (gethash type *types*) (call-next-method))))

(defmethod type->llvm ((type adt))
  (let* ((def (adt-def type))
         (args (adt-args type))
         (tvars (tvars def))
         (map (hare::make-tysubst (mapcar #'cons tvars args)))
         (structs
           (loop for constructor in (constructors def)
                 collect (constructor->llvm constructor map)))
         (nstructs (length structs)))
    (case nstructs
      (0 (let ((st (llvm:struct-create-named "")))
           (llvm:struct-set-body st nil)
           st))
      (1 (first structs))
      (otherwise
       (error "unions are hard :(")))))
