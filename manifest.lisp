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

(defmethod manifest-ast ((ast branch) tysubst)
  (make-instance 'branch
    :test (manifest-ast (test ast) tysubst)
    :then (manifest-ast (then ast) tysubst)
    :else (manifest-ast (else ast) tysubst)
    :type (subst-type tysubst (type ast))))

(defmethod manifest-ast ((ast bind) tysubst)
  (make-instance 'bind
    :variable (variable ast)
    :value (manifest-ast (value ast) tysubst)
    :body (manifest-ast (body ast) tysubst)))

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
    :adt-def (adt-def ast) :case!p (case!p ast)))

;;;

;;; A monotyped variable.
(defclass manifestation ()
  ((%name :initarg :name :reader name :type string)
   (%variable :initarg :variable :reader variable)
   (%initializer :initarg :initializer :reader initializer :type initializer)))

;;; A variable that is defined elsewhere, but still has a monotype.
(defclass extern ()
  ((%name :initarg :name :reader name :type string)
   (%variable :initarg :variable :reader variable)
   (%type :initarg :type :reader type)))

(defclass manifest ()
  ((%manifestations :initarg :manifestations :reader manifestations :type list)
   (%externs :initarg :externs :reader externs :type list)))

(defun %find-manifest (variable type manifestations)
  (find-if (lambda (manifest)
             (and (eq (variable manifest) variable)
                  (type= (type manifest) type)))
           manifestations))

(defun mangle (name type)
  ;; FIXME
  (format nil "~a_~a" name (unparse-type type)))

(defun %manifest (module particulars) ; alist of (variable monotype [c-name])
  (declare (optimize debug))
  (loop with complete = nil with externs = nil
        with worklist = particulars
        with entries = (entries module)
        for (var type name) = (or (pop worklist)
                                  (return (values complete externs)))
        for entry = (find var entries :key #'variable)
        if entry
          do (unless (%find-manifest var type complete)
               (let* ((einitializer (initializer entry))
                      (tysubst (unify type (type einitializer)))
                      (initializer (manifest-initializer einitializer tysubst))
                      (infer (inference entry))
                      (new (make-instance 'manifestation
                             :name (or name (mangle (name var) type))
                             :variable var :initializer initializer))
                      (varmap (varmap infer))
                      (svarmap (subst-map tysubst varmap)))
                 ;; FIXME? We're treating a varmap as an alist directly here,
                 ;; breaking abstraction
                 (loop for (vvar . vtypes) in svarmap
                       do (loop for vtype1 in vtypes
                                for vtype2 = (pointer-type-underlying vtype1)
                                do (push (list vvar vtype2) worklist)))
                 (push new complete)))
        else
          do (unless (%find-manifest var type externs)
               (push (make-instance 'extern
                       :name (or name (mangle (name var) type))
                       :variable var :type type)
                     externs))))

(defun manifest (module particulars)
  (multiple-value-bind (manifestations externs) (%manifest module particulars)
    (make-instance 'manifest :manifestations manifestations :externs externs)))
