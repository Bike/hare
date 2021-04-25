(in-package #:hare.type)

;;; Definition of a particular constructor in an ADT. Not a type.
(defclass constructor ()
  ((%name :initarg :name :reader name :type symbol)
   (%adt-def :initarg :adt-def :reader adt-def :type adt-def)
   ;; A proper list of TYPEs
   (%fields :initarg :fields :accessor fields :type list)))

(defmethod print-object ((o constructor) s)
  (print-unreadable-object (o s :type t) (write (name o) :stream s)))

;;; Definition of an ADT schema thing, as from defadt. Not a type.
(defclass adt-def ()
  ((%name :initarg :name :accessor name :type symbol)
   ;; A proper list of type variables bound by this definition (TVARs)
   (%tvars :initarg :tvars :accessor tvars :type list)
   ;; A proper list of CONSTRUCTORs
   (%constructors :initarg :constructors :accessor constructors :type list)))

(defun arity (adt-def) (length (tvars adt-def)))

;;; E.g., if we have (defadt foo (x) ...), and then refer to (foo (int 3))
;;; somewhere, we have one of these, with args = ((int 3)).
;;; In type theory this is more often called TApp or the like.
(defclass adt (type)
  (;; Which ADT definition this is an instantiation of.
   (%def :initarg :def :accessor adt-def :type adt-def)
   ;; A list of types.
   (%args :initarg :args :accessor adt-args :type list)))
(defun make-adt (def args)
  (let ((key (list* def args)))
    (or (cached-adt key)
        (setf (cached-adt key) (make-instance 'adt :def def :args args)))))
(defmethod type= ((t1 adt) (t2 adt))
  (and (eq (adt-def t1) (adt-def t2))
       ;; adt defs are the same, so the arglists have the same length.
       (every #'type= (adt-args t1) (adt-args t2))))
(defmethod map-type (function (type adt))
  (make-adt (adt-def type)
            (loop for ty in (adt-args type)
                  collect (map-type function ty))))
(defmethod mapnil-type (function (type adt))
  (loop for ty in (adt-args type) do (map-type function ty)))
(defmethod unparse-type ((type adt))
  `(,(name (adt-def type)) ,@(mapcar #'unparse-type (adt-args type))))

;;;

;;; INERT is a slightly magical ADT.
;;; It could be defined by (defadt inert () (inert)), but we use it within the
;;; compiler, so it's made/stored ahead of time.
(defvar *inert-def*
  (let ((def (make-instance 'adt-def
               :name 'inert :tvars nil)))
    (setf (constructors def) (list (make-instance 'constructor
                                     :name 'inert :adt-def def
                                     :fields ())))
    def))

(defun inert () (make-adt *inert-def* nil))

;;;

;;; This is a bit like instantiating a polytype.
;;; Given a constructor, we return an ADT type and a list of fields
;;; for that constructor. All of these have fresh type variables substituted
;;; for all of the ADT definition's. This is useful in infer.lisp.
(defun instantiate-constructor (constructor)
  (let* ((adt-def (adt-def constructor))
         (old (tvars adt-def))
         (new (loop for tvar in old collect (make-tvar (name tvar))))
         (map (mapcar #'cons old new))
         (tysubst (make-tysubst map)))
    (values
     (make-adt adt-def new)
     (loop for field in (fields constructor)
           collect (subst-type tysubst field)))))

;;; Same but do all constructors, and return as a (constructor . types) alist.
(defun instantiate-adt-def (adt-def)
  (let* ((old (tvars adt-def))
         (new (loop for tvar in old collect (make-tvar (name tvar))))
         (map (mapcar #'cons old new))
         (tysubst (make-tysubst map)))
    (values
     (make-adt adt-def new)
     (loop for constructor in (constructors adt-def)
           for fieldtys = (loop for field in (fields constructor)
                                collect (subst-type tysubst field))
           collect (cons constructor fieldtys)))))

;;;

(defun subst-constructor (tysubst constructor)
  (make-instance 'constructor
    :name (name constructor) :adt-def (adt-def constructor)
    :fields (loop for field in (fields constructor)
                  collect (subst-type tysubst field))))
;;; Compute copies of the constructors without the def's tvars free.
;;; I don't think we can compute this up front because it would result in
;;; baseless recursion when an ADT is recursive. I'm sure we can do this in a
;;; way smarter than this, though - we do much recomputation.
(defmethod constructors ((ty adt))
  (let* ((def (adt-def ty)) (tvars (tvars def)) (args (adt-args ty))
         (map (make-tysubst (mapcar #'cons tvars args))))
    (loop for constructor in (constructors def)
          collect (subst-constructor map constructor))))
