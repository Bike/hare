(in-package #:hare)

(defgeneric manifest-initializer (object tysubst))
(defgeneric manifest-ast (ast tysubst))

(defmethod manifest-initializer ((object integer-initializer) tysubst)
  (make-instance 'integer-initializer :value (value object)
                 :type (subst-type tysubst (type object))))

(defmethod manifest-initializer ((object variable-initializer) tysubst)
  (make-instance 'variable-initializer :variable (variable object)
                 :type (subst-type tysubst (type object))))

(defun subst-type-list (tysubst list-of-types)
  (loop for type in list-of-types
        collect (subst-type tysubst type)))

(defun manifest-constructor (constructor tysubst)
  (make-instance 'constructor
    :name (name constructor) :adt-def (adt-def constructor)
    :fields (subst-type-list tysubst (fields constructor))))

(defmethod manifest-initializer ((object constructor-initializer) tysubst)
  (make-instance 'constructor-initializer
    :constructor (manifest-constructor (constructor object) tysubst)
    :fields (loop for field in (fields object)
                  collect (manifest-initializer field tysubst))
    :type (subst-type tysubst (type object))))

(defmethod manifest-initializer ((object undef-initializer) tysubst)
  (make-instance 'undef-initializer :type (subst-type tysubst (type object))))

(defmethod manifest-initializer ((object lambda-initializer) tysubst)
  (make-instance 'lambda-initializer :params (params object)
                 :body (manifest-ast (body object) tysubst)
                 :type (subst-type tysubst (type object))))

(defmethod manifest-initializer ((object array-initializer) tysubst)
  (make-instance 'array-initializer
    :elements (loop for element in (elements object)
                    collect (manifest-initializer element tysubst))
    :type (subst-type tysubst (type object))))

(defun manifest-ast-list (asts tysubst)
  (loop for ast in asts collect (manifest-ast ast tysubst)))

(defmethod manifest-ast ((ast seq) tysubst)
  (make-instance 'seq
    :asts (manifest-ast-list (asts ast) tysubst)
    :type (subst-type tysubst (type ast))))

(defmethod manifest-ast ((ast call) tysubst)
  (make-instance 'call
    :callee (manifest-ast (callee ast) tysubst)
    :args (manifest-ast-list (args ast) tysubst)
    :type (subst-type tysubst (type ast))))

(defmethod manifest-ast ((ast literal) tysubst)
  (make-instance 'literal
    :initializer (manifest-initializer (initializer ast) tysubst)
    :type (subst-type tysubst (type ast))))

(defmethod manifest-ast ((ast reference) tysubst)
  (make-instance 'reference
    :variable (variable ast) :type (subst-type tysubst (type ast))))

(defmethod manifest-ast ((ast bind) tysubst)
  (make-instance 'bind
    :variable (variable ast)
    :value (manifest-ast (value ast) tysubst)
    :body (manifest-ast (body ast) tysubst)
    :type (subst-type tysubst (type ast))))

(defun manifest-case-clause (clause tysubst)
  (make-instance 'case-clause
    :constructor (manifest-constructor (constructor clause) tysubst)
    :variables (variables clause)
    :body (manifest-ast (body clause) tysubst)))

(defmethod manifest-ast ((ast case) tysubst)
  (make-instance 'case
    :value (manifest-ast (value ast) tysubst)
    :clauses (loop for clause in (clauses ast)
                   collect (manifest-case-clause clause tysubst))
    :adt-def (adt-def ast) :case!p (case!p ast)
    :type (subst-type tysubst (type ast))))

(defmethod manifest-ast ((ast construct) tysubst)
  (make-instance 'construct
    :constructor (constructor ast)
    :args (loop for arg in (args ast)
                collect (manifest-ast arg tysubst))
    :type (subst-type tysubst (type ast))))

;;;

(defun check-initializer-typed (initializer)
  (let ((f (free (type initializer))))
    (unless (null f)
      (error "Initializer ~a incompletely manifested as type ~a"
             initializer f))))

;;;

;;; Abstract.
(defclass manifestation ()
  ((%name :initarg :name :initform nil :accessor name :type (or string null))
   (%variable :initarg :variable :reader variable :type variable)))

(defmethod print-object ((o manifestation) s)
  (print-unreadable-object (o s :type t)
    (write (name (variable o)) :stream s)))

;;; A monotyped variable.
(defclass monodef (manifestation)
  ((%initializer :initarg :initializer :reader initializer :type initializer)))

(defmethod type ((md monodef)) (type (initializer md)))

;;; A variable that is defined elsewhere, but still has a monotype.
(defclass extern (manifestation)
  ((%type :initarg :type :reader type)))

(defclass manifest ()
  ((%monodefs :initarg :monodefs :reader monodefs :type list)
   (%externs :initarg :externs :reader externs :type list)))

(defun %find-manifest (variable type monodefs)
  (declare (optimize debug))
  (find-if (lambda (manifest)
             (and (eq (variable manifest) variable)
                  (type= (type manifest) type)))
           monodefs))

(defun mangle (name type)
  ;; FIXME
  (format nil "~a_~a" name (unparse-type type)))

(defun %manifest (module particulars) ; alist of (variable monotype [c-name])
  (declare (optimize debug))
  (loop with complete = nil with externs = nil
        with worklist = particulars
        with entries = (entries module)
        for (var type name)
          = (or (pop worklist)
                (progn
                  (loop for mono in complete
                        unless (name mono)
                          do (setf (name mono)
                                   (mangle (name (variable mono))
                                           (type (initializer mono)))))
                  (loop for extern in externs
                        unless (name extern)
                          do (setf (name extern)
                                   (mangle (name (variable extern))
                                           (type extern))))
                  (return (values complete externs))))
        for entry = (find var entries :key #'variable)
        if entry
          do (let ((existing (%find-manifest var type complete)))
               (if existing
                   ;; maybe set name
                   (when name
                     (if (name existing)
                         (error "Duplicate names for ~a" var)
                         (setf (name existing) name)))
                   ;; new entry
                   (let* ((einitializer (initializer entry))
                          (tysubst (unify type (type einitializer)))
                          (initializer
                            (manifest-initializer einitializer tysubst))
                          (infer (inference entry))
                          (new (make-instance 'monodef
                                 :name name
                                 :variable var :initializer initializer))
                          (varmap (varmap infer))
                          (svarmap (subst-map tysubst varmap)))
                     (check-initializer-typed initializer)
                     ;; FIXME? We're treating a varmap as an alist directly
                     ;; here, breaking abstraction
                     (loop for (vvar . vtypes) in svarmap
                           do (loop for vtype1 in vtypes
                                    for vtype2 = (pointer-type-underlying
                                                  vtype1)
                                    do (push (list vvar vtype2) worklist)))
                     (push new complete))))
        else
          do (let ((existing (%find-manifest var type externs)))
               (if existing
                   ;; maybe set the name
                   (when name
                     (if (name existing)
                         (error "Duplicate names for ~a" var)
                         (setf (name existing) name)))
                   ;; make new extern
                   (push (make-instance 'extern
                           :name name
                           :variable var :type type)
                         externs)))))

(defun manifest (module particulars)
  (multiple-value-bind (monodefs externs) (%manifest module particulars)
    (make-instance 'manifest :monodefs monodefs :externs externs)))
