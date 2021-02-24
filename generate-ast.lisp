(in-package #:hare)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse forms
;;;

(defun convert (form env type-env)
  (etypecase form
    (symbol (convert-symbol form env type-env))
    (cons (convert-cons (first form) (rest form) env type-env))
    (t (convert-literal form env type-env))))

(defun convertlis (list env type-env)
  (loop for form in list
        collect (convert form env type-env)))

(defun convert-symbol (symbol env type-env)
  (let ((thing (lookup form env)))
    (etypecase thing
      (variable (make-instance 'reference :variable thing))
      (initializer (make-instance 'literal :initializer thing)))))

(defun convert-cons (head args env type-env)
  (cl:case head
    ((seq) (convertlis args env type-env))
    ((let)
     (destructuring-bind ((var value) &rest body) args
       (let* ((lvar (make-variable var))
              (new-env (make-env (list var) (list lvar) env)))
         (make-instance 'bind
           :var lvar :value (convert value env type-env)
           :body (convertlis body new-env type-env)))))
    ((if)
     (destructuring-bind (test then else) args
       (make-instance 'branch
         :test (convert test env type-env)
         :then (convert then env type-env)
         :else (convert else env type-env))))
    ((with)
     (destructuring-bind ((var &optional (initializer nil initializerp))
                          &rest body)
         args
       (let ((lvar (make-variable var))
             (initializer
               (if initializerp
                   ;; FIXME: Should be a global env probably
                   (parse-initializer initializer env type-env)
                   (undef))))
         (make-instance 'with
           :var lvar
           :initialization (make-instance 'initialization
                             :initializer initializer)
           :body (convertlis
                  body (make-env (list var) (list lvar) env) type-env)))))
    #+(or)
    ((with-array)
     (destructuring-bind ((var len) &rest body) args
       (let ((lvar (make-variable var)))
         (make-instance 'with
           :var (make-variable var) :len (convert len env type-env)
           :body (convert body (acons var lvar env) type-env)))))
    ((case case!)
     (destructuring-bind (value &rest cases) args
       (when (null cases)
         (error "Empty case"))
       (multiple-value-bind (def cases)
           (case-adt-def cases type-env)
         (let ((cases
                 (loop for ((constructor . vars) . body) in cases
                       for lvars = (mapcar #'make-variable vars)
                       for new-env = (make-env vars lvars env)
                       collect (cons (cons constructor lvars)
                                     (convertlis body new-env type-env)))))
           (make-instance 'case
             :value (convert value env type-env)
             :cases cases
             :case!p (eq head 'case!)
             :adt-def def)))))
    ((quote)
     (destructuring-bind (spec) args
       (make-instance 'literal
         ;; Should be a global env
         :initializer (parse-literal spec env type-env))))
    (otherwise ; call
     (make-instance 'call
       :callee (convert head env type-env)
       :args (loop for form in args
                   collect (convert form env type-env))))))

(defun convert-literal (form env type-env)
  (make-instance 'literal
    :initializer (parse-literal form env type-env)))

;;; Given the ((constructor var*) . ast)* list from a case,
;;; return the appropriate adt def, and order the cases to match the def.
(defun case-adt-def (cases type-env)
  (let* ((constructors (mapcar #'caar cases))
         (def (find-adt-def-from-constructor (first constructors) type-env))
         (oconstructors (constructors def)))
    (unless (null (set-exclusive-or constructors oconstructors :test #'eq))
      ;; FIXME: improve message
      (error "Case mismatch ~a ~a" constructors oconstructors))
    (values def
            ;; Rearrange the cases.
            (loop for oconstructor in oconstructors
                  collect (find oconstructor cases
                                :key #'caar :test #'eq)))))
