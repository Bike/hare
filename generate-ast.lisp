(in-package #:hare)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse forms
;;;

;;; FIXME: Should we really handle case like this? I'm not sure.
(defun parse-form (form env adt-env)
  (flet ((parse-seq (list env)
           (make-instance 'seq
             :asts (loop for form in list
                         collect (parse-form form env adt-env)))))
    (etypecase form
      (symbol
       (let ((thing (lookup form env)))
         (etypecase thing
           (variable (make-instance 'reference :variable thing))
           (initializer (make-instance 'literal :initializer thing)))))
      (cons
       (let ((head (car form)) (args (cdr form)))
         (cl:case head
           ((seq) (parse-seq args env))
           ((let)
            (destructuring-bind ((var value) &rest body) args
              (let ((lvar (make-variable var)))
                (make-instance 'bind
                  :var lvar :value (parse-form value env adt-env)
                  :body (parse-seq body (acons var lvar env))))))
           ((if)
            (destructuring-bind (test then else) args
              (make-instance 'branch
                :test (parse-form test env adt-env)
                :then (parse-form then env adt-env)
                :else (parse-form else env adt-env))))
           ((with)
            (destructuring-bind ((var &optional (initializer nil initializerp))
                                 &rest body)
                args
              (let ((lvar (make-variable var))
                    (initializer
                      (if initializerp
                          (parse-initializer initializer env adt-env)
                          (undef))))
                (make-instance 'with
                  :var lvar
                  :initialization (make-instance 'initialization
                                    :initializer initializer)
                  :body (parse-seq
                         body (make-env (list var) (list lvar) env))))))
           #+(or)
           ((with-array)
            (destructuring-bind ((var len) &rest body) args
              (let ((lvar (make-variable var)))
                (make-instance 'with
                  :var (make-variable var) :len (parse-form len env adt-env)
                  :body (parse-seq body (acons var lvar env))))))
           ((case case!)
            (destructuring-bind (value &rest cases) args
              (when (null cases)
                (error "Empty case"))
              (multiple-value-bind (def cases)
                  (case-adt-def cases adt-env)
                (let ((cases
                        (loop for ((constructor . vars) . body) in cases
                              for lvars = (mapcar #'make-variable vars)
                              for new-env = (make-env vars lvars env)
                              collect (cons (cons constructor lvars)
                                            (parse-seq body new-env)))))
                  (make-instance 'case
                    :value (parse-form value env adt-env)
                    :cases cases
                    :case!p (eq head 'case!)
                    :adt-def def)))))
           ((quote)
            (destructuring-bind (spec) args
              (make-instance 'literal
                :initializer (parse-literal spec env adt-env))))
           (otherwise ; call
            (make-instance 'call
              :callee (parse-form head env adt-env)
              :args (loop for form in args
                          collect (parse-form form env adt-env)))))))
      ;; Back to the typecase - remember that? So long ago
      (t
       (make-instance 'literal
         :initializer (parse-literal form env adt-env))))))

;;; Given the ((constructor var*) . ast)* list from a case,
;;; return the appropriate adt def, and order the cases to match the def.
(defun case-adt-def (cases adt-env)
  (let* ((constructors (mapcar #'caar cases))
         (def (find-adt-def-from-constructor (first constructors) adt-env))
         (oconstructors (constructors def)))
    (unless (null (set-exclusive-or constructors oconstructors :test #'eq))
      ;; FIXME: improve message
      (error "Case mismatch ~a ~a" constructors oconstructors))
    (values def
            ;; Rearrange the cases.
            (loop for oconstructor in oconstructors
                  collect (find oconstructor cases
                                :key #'caar :test #'eq)))))
