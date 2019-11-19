(in-package #:hare)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; C translation
;;;

;;; Given a type, return a partial name string for it.
;;; We pretend _ is reserved and use _c to mean special things,
;;; c being some character. Since our types are complicated.
;;; We don't actually need to unparse any of these,
;;; at least for the moment. They just need to be consistent
;;; and unique (i.e. bijective to the actual types).
(defgeneric mangle-type (type))
(defmethod mangle-type ((type int))
  ;; Write int lengths out. In hex. maybe a little contrarian.
  (format nil "i~x" (int-type-length type)))
(defmethod mangle-type ((type pointer))
  (format nil "pointer_~a" (pointer-type-underlying type)))
(defmethod mangle-type ((type fun))
  (concatenate 'string "function"
               (apply #'concatenate 'string
                      "_r" (mangle-type (fun-return type))
                      (loop for p in (parameters type)
                            for i from 0
                            collect (format nil "_a~x~a"
                                            i (mangle-type p))))
               ;; completion marker
               "_d"))
(defmethod mangle-type ((type arrayt))
  (format nil "array_~a" (mangle-type (arrayt-element-type type))))
(defun adt-c-name (adt-name types)
  (concatenate 'string
               (string-downcase (symbol-name adt-name))
               (apply #'concatenate 'string
                      (loop for a in types
                            for i from 0
                            collect (format nil "_a~x~a"
                                            i (mangle-type a))))
               "_d"))
(defmethod mangle-type ((type adt))
  (concatenate 'string
               "struct "
               (adt-c-name (name (adt-def type)) (adt-args type))))

;;; Given an ADT definition and types for its variables, return a C name.
(defun mangle-adt-def (adt-def types)
  (concatenate 'string
               (string-downcase (symbol-name (name adt-def)))
               (apply #'concatenate 'string
                      (loop for a in types
                            for i from 0
                            collect (format nil "_a~x~a"
                                            i (mangle-type a))))
               "_d"))

;;; Given a string variable name, and a type,
;;; return a declaration of the variable.
;;; NOTE: I'd like to just have the c-type-name function,
;;; but thanks to declaration-follows-use this is not sane in general.
(defgeneric c-variable-declaration (name type)
  (:argument-precedence-order type name))

;;; Return a C string representing the given type.
;;; Not useful for some types (e.g. functions)
(defgeneric c-type-name (type))

(defmethod c-type-name ((type int))
  (let ((len (int-type-length type)))
    (cond ((<= len 8) "uint_least8_t")
          ((<= len 16) "uint_least16_t")
          ((<= len 32) "uint_least32_t")
          ((<= len 64) "uint_least64_t")
          (t (error "int type not supported: ~d is too many bits"
                    len)))))

(defun easy-declaration (name type)
  (format nil "~s ~s;" (c-type-name type) name))
(defmethod c-variable-declaration (name (type int))
  (easy-declaration name type))

(defun function-pointer-declaration (name return-type param-types
                                     nptr arrayp)
  (format nil ))

(defmethod c-type-name ((type pointer))
  ;; Does not work for function pointers
  (format nil "~s*" (c-type-name (pointer-type-underlying type))))
(defmethod c-variable-declaration ((type pointer))
  (let ((under 
(defmethod c-variable-declaration ((type fun)) ...)
(defmethod c-variable-declaration (name (type arrayt))
  ;; NOTE: This only makes sense when laying out a struct-
  ;; with-array has to be handled more specially.
  ;; FIXME: Arrays of function pointers are an urban legend
  (format nil "~s ~s[];" (c-type-name (arrayt-element-type type)) name))
(defmethod c-type-name ((type adt))
  (format nil "struct ~s"
          (adt-c-name (name (adt-def type)) (adt-args type))))

(defmethod c-variable-declaration (name (type adt))
  (easy-declaration name type))

;;; Given an ADT's c-name, constructors, and members, return a C type
;;; declaration for it. All type variables must have been substituted
;;; already at this point.
(defun adt-def->c-aux (c-name constructors members)
  (flet ((constructor-struct-name (name types)
           (concatenate 'string
                        "struct "
                        (string-downcase
                         (symbol-name name))
                        " { "
                        (apply #'concatenate
                               'string
                               (loop for i from 0
                                     for type in types
                                     collect (c-type-name type)
                                     collect (format nil " a~x; " i)))
                        "}")))
  (let* ((consstructs ; constructor structs
           (loop for constructor in constructors
                 for types in members
                 collect (constructor-struct-name constructor types)))
         (union
           (concatenate
            'string
            "union { "
            (apply #'concatenate
                   'string
                   (loop for i from 0
                         for consstruct in consstructs
                         collect consstruct
                         collect (format nil " u~x; " i)))
            "}")))
    (concatenate 'string "struct "
                 c-name " { "
                 (if (> (length consstructs) 1) ; we need to disambiguate.
                     "int tag; "
                     "")
                 union " dat; };"))))
(defun adt-def->c (adt-def types)
  (let ((subst (mapcar #'cons (tvars adt-def) types))
        (c-name (adt-c-name (name adt-def) types)))
    (adt-def->c-aux
     c-name
     (constructors adt-def)
     (loop for member in (members adt-def)
           collect (loop for type in member
                         collect (subst-type subst type))))))

;;; Generate C code to compute the expression.
;;; Store the result in destvar (a string).
;;; Returns statements, not an expression, because C.
(defgeneric translate (ast destvar))

(defun symbol->c (symbol)
  (string-downcase (substitute #\_ #\- (symbol-name symbol))))

(defmethod translate ((ast global) destvar)
  (format nil "~s = ~s;" destvar (symbol->c (name ast))))

(defmethod translate ((ast branch) destvar)
  (format nil "{ bool cond; ~s if (cond) { ~s } else { ~s } }"
          (translate (test branch) "cond")
          (translate (then branch) destvar)
          (translate (else branch) destvar)))

(defmethod translate ((ast bind) destvar)
  (multiple-value-bind (declaration c-var-name)
      (let ((var (var ast)))
        (c-variable-declaration (name var) (type var)))
    (format nil "{ ~s ~s ~s }"
            declaration (translate (value ast) c-var-name)
            (translate (body ast) destvar))))
