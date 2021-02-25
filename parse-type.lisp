(in-package #:hare)

;;; Type environments are as in type-env.lisp.
(defun parse-type (expr type-env)
  (etypecase expr
    (symbol ; alias or zero-arg adt.
     (let ((alias (find-alias expr type-env)))
       (if alias
           (progn (assert (null (first alias)))
                  (second alias))
           ;; "foo" can be short for "(foo)"
           (let ((adt-def (find-adt-def expr type-env)))
             (assert (zerop (arity adt-def)))
             (make-adt adt-def nil)))))
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
        (let ((alias (find-alias (car expr) type-env))
              (rest (loop for type in (cdr expr)
                          collect (parse-type type type-env))))
          (if alias
              (progn
                (assert (= (length rest) (length (first alias))))
                (subst-type
                 (mapcar #'cons (first alias) rest) (second alias)))
              (let ((adt-def (find-adt-def (car expr) type-env)))
                (assert (= (arity adt-def) (length rest)))
                (make-adt adt-def rest)))))))))
