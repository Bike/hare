(in-package #:hare)

(defclass manifestation ()
  ((%variable :initarg :variable :reader variable)
   (%type :initarg :type :reader type)))

(defclass manifest ()
  ((%manifestations :initarg :manifestations :reader manifestations :type list)
   (%externs :initarg :externs :reader externs :type list)))

(defun %find-manifest (variable type manifestations)
  (find-if (lambda (manifest)
             (and (eq (variable manifest) variable)
                  (type= (type manifest) type)))
           manifestations))

(defun %manifest (module alist) ; alist of (variable . monotype)
  (loop with complete = nil with externs = nil
        with worklist = alist
        with entries = (entries module)
        for (var . type) = (or (pop worklist)
                               (return (values complete externs)))
        for entry = (find var entries :key #'variable)
        if entry
          do (unless (%find-manifest var type complete)
               (let* ((new (make-instance 'manifestation
                             :variable var :type type))
                      (tysubst (unify type (type (initializer entry))))
                      (infer (inference entry))
                      (varmap (varmap infer))
                      (svarmap (subst-map tysubst varmap)))
                 ;; FIXME? We're treating a varmap as an alist directly here,
                 ;; breaking abstraction
                 (loop for (vvar . vtypes) in svarmap
                       do (loop for vtype in vtypes
                                do (push (cons vvar vtype) worklist)))
                 (push new complete)))
        else
          do (unless (%find-manifest var type externs)
               (push (make-instance 'manifestation
                       :variable var :type type)
                     externs))))

(defun manifest (module alist)
  (multiple-value-bind (manifestations externs) (%manifest module alist)
    (make-instance 'manifest :manifestations manifestations :externs externs)))
