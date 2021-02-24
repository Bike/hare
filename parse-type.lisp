(in-package #:hare)

;;; Type environments are as in type-env.lisp.
(defun parse-type (expr type-env)
  (etypecase expr
    (symbol ; alias or zero-arg adt.
     (or (find-type expr type-env)
         ;; "foo" can be short for "(foo)"
         (let ((adt-def (find-adt-def expr type-env)))
           (assert (zerop (arity adt-def)))
           (make-adt adt-def nil))))
    (cons
     (cl:case (car expr)
       ((int)
        (destructuring-bind (len) (cdr expr)
          (check-type len (integer 0))
          (make-int len)))
       ((pointer)
        (destructuring-bind (under) (cdr expr)
          (make-pointer (parse-type under type-env))))
       ((array)
        (destructuring-bind (et) (cdr expr)
          (make-arrayt (parse-type et type-env))))
       ((function)
        (destructuring-bind (ret &rest params) (cdr expr)
          (make-fun (parse-type ret type-env)
                    (loop for param in params
                          collect (parse-type param type-env)))))
       (otherwise
        (let ((adt-def (find-adt-def (car expr) type-env)))
          (assert (= (arity adt-def) (length (cdr expr))))
          (make-adt adt-def
                    (loop for type in (cdr expr)
                          collect (parse-type type type-env)))))))))
