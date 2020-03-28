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

(defmethod type->llvm ((type arrayt))
  (llvm:array-type
   (type->llvm (arrayt-element-type type))
   ;; zero arrays are the accepted way to mean variable arrays.
   0))

(defmethod type->llvm ((type adt))
  (let* ((def (adt-def type))
         (args (adt-args type))
         (tvars (tvars def))
         (map (mapcar #'cons tvars args))
         (structs
           (loop for member in (members def)
                 for constructor in (constructors def)
                 for smember = (loop for mtype in member
                                     collect (subst-type map mtype))
                 for struct = (llvm:struct-create-named
                               (string-downcase (symbol-name constructor)))
                 do (llvm:struct-set-body struct smember)
                 collect struct))
         (nstructs (length structs)))
    (case nstructs
      (0 (let ((st (llvm:struct-create-named "")))
           (llvm:struct-set-body st nil)
           st))
      (1 (first structs))
      (otherwise
       (error "unions are hard :(")))))
