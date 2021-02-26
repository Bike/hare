(in-package #:hare)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type schemata
;;; AKA polytypes. They are NOT actually types.
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

;;; HM operation. Given a schema, return a type with fresh free variables
;;; for all the schema's old bound variables.
(defun instantiate (schema)
  ;; could use subst-type for this
  (let ((map (loop for tvar in (tvars schema)
                   collect (cons tvar (make-tvar (name tvar))))))
    (map-type (lambda (type)
                (when (typep type 'tvar)
                  (let ((pair (assoc type map :test #'eq)))
                    (if pair (cdr pair) nil))))
              (type schema))))

;;; This is a bit like instantiating a polytype, so it's here.
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
