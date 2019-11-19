(in-package #:hare)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse forms
;;;

;;; env is an alist (symbol . local). Globals should NOT be included.
;;; adt-env is the same alist (name . adt-def) as in parse-type.
;;; Should we really handle case like this? I'm not sure.
(defun parse-form (form env adt-env)
  (flet ((parse-seq (list env)
           (make-instance 'seq
             :asts (loop for form in list
                         collect (parse-form form env adt-env))))
         (local (name)
           (check-type name symbol)
           (make-instance 'local :name name)))
    (etypecase form
      (symbol
       (or (cdr (assoc form env :test #'eq))
           (make-instance 'global :name form)))
      (cons
       (let ((head (car form)) (args (cdr form)))
         (cl:case head
           ((seq) (parse-seq args env))
           ((let)
            (destructuring-bind ((var value) &rest body) args
              (let ((lvar (local var)))
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
            (destructuring-bind ((var) &rest body) args
              (let ((lvar (local var)))
                (make-instance 'with
                  :var lvar :len nil
                  :body (parse-seq body (acons var lvar env))))))
           ((with-array)
            (destructuring-bind ((var len) &rest body) args
              (let ((lvar (local var)))
                (make-instance 'with
                  :var (local var) :len (parse-form len env adt-env)
                  :body (parse-seq body (acons var lvar env))))))
           ((case case!)
            (destructuring-bind (value &rest cases) args
              (multiple-value-bind (def cases)
                  (find-adt-def cases adt-env)
                (let ((cases
                        (loop for ((constructor . vars) . body) in cases
                              for lvars = (mapcar #'local vars)
                              for env = (mapcar #'cons vars lvars)
                              collect (cons (cons constructor lvars)
                                            (parse-seq body env)))))
                  (make-instance 'case
                    :value (parse-form value env adt-env)
                    :cases cases
                    :case!p (eq head 'case!)
                    :adt-def def)))))
           (otherwise ; call
            (make-instance 'call
              :callee (parse-form head env adt-env)
              :args (loop for form in args
                          collect (parse-form form env adt-env)))))))
      ;; Back to the typecase - remember that? So long ago
      ((and (integer 0) fixnum)
       (make-instance 'numeric-literal :value form)))))

;;; Given the ((constructor var*) . ast)* list from a case,
;;; return the appropriate adt def, and order the cases to match the def.
(defun find-adt-def (cases env)
  (let* ((constructors (mapcar #'caar cases))
         (def (loop for (ignore . def) in env
                    for oconstructors = (constructors def)
                    ;; when (set-equal constructors oconstructors)
                    when (null (set-exclusive-or constructors oconstructors
                                                 :test #'eq))
                      return def
                    finally (error 'case-unknown :constructors constructors))))
    (values def
            ;; Rearrange the cases.
            (loop for oconstructor in (constructors def)
                  collect (find oconstructor cases
                                :key #'caar :test #'eq)))))
