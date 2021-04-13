(in-package #:hare)

(defun manifest-initializer (initializer tysubst)
  (let ((copy (ast:copy-initializer initializer)))
    (ast:mapnil-initializer
     (lambda (o) (setf (ast:type o) (type:subst-type tysubst (ast:type o))))
     copy)
    copy))

;;;

(defun check-initializer-typed (initializer)
  (let ((f (type:free (ast:type initializer))))
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
  ((%initializer :initarg :initializer :reader initializer
                 :type ast:initializer)))

(defmethod type ((md monodef)) (ast:type (initializer md)))

;;; A variable that is defined elsewhere, but still has a monotype.
(defclass extern (manifestation)
  ((%type :initarg :type :reader type :type type:type)))

(defclass manifest ()
  ((%monodefs :initarg :monodefs :reader monodefs :type list)
   (%externs :initarg :externs :reader externs :type list)))

(defun %find-manifest (variable type monodefs)
  (declare (optimize debug))
  (find-if (lambda (manifest)
             (and (eq (variable manifest) variable)
                  (type:type= (type manifest) type)))
           monodefs))

(defun mangle-count (n)
  (check-type n (integer 0))
  (let* ((str (write-to-string n :base 16))
         (L (length str)))
    (check-type L (integer 0 15))
    (concatenate 'string (write-to-string L :base 16) str)))

(defgeneric mangle-type (type))
(defmethod mangle-type ((type type:int))
  (concatenate 'string "i" (mangle-count (type:int-type-length type))))
(defmethod mangle-type ((type type:pointer))
  (concatenate 'string "p" (mangle-type (type:pointer-type-underlying type))))
(defmethod mangle-type ((type type:fun))
  (apply #'concatenate 'string "f"
         (mangle-count (length (type:parameters type)))
         (mangle-type (type:fun-return type))
         (mapcar #'mangle-type (type:parameters type))))
(defmethod mangle-type ((type type:arrayt))
  (concatenate 'string "a" (mangle-type (type:arrayt-element-type type))))
(defmethod mangle-type ((type type:adt))
  (let* ((def (type:adt-def type))
         (name (type:name def))
         (sname (string-downcase (write-to-string name)))
         (Lname (length sname))
         (args (type:adt-args type)))
    (apply #'concatenate 'string "c" ; custom, since A is taken
           (mangle-count Lname)
           sname
           (mangle-count (length args))
           (mapcar #'mangle-type args))))

(defun mangle (name type)
  ;; FIXME
  (format nil "~a_~a" name (mangle-type type)))

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
                                   (mangle (ast:name (variable mono))
                                           (ast:type (initializer mono)))))
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
                          (tysubst (unify type (ast:type einitializer)))
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
                                    for vtype2 = (type:pointer-type-underlying
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
