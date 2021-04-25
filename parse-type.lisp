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
             (cond (adt-def
                    (assert (zerop (type:arity adt-def)))
                    (type:make-adt adt-def nil))
                   (t (let ((ph (make-instance 'type:unknown
                                  :form expr
                                  :type-env (lexical-type-env type-env))))
                        (restart-case
                            (error 'unknown-type :name expr :placeholder ph)
                          (continue () ph)))))))))
    (cons
     (cl:case (car expr)
       ((int)
        (destructuring-bind (len) (cdr expr)
          (check-type len (integer 0))
          (type:make-int len)))
       ((pointer)
        (destructuring-bind (under) (cdr expr)
          (type:make-pointer (parse-type under type-env))))
       ((array)
        (destructuring-bind (et) (cdr expr)
          (type:make-arrayt (parse-type et type-env))))
       ((function)
        (destructuring-bind (ret &rest params) (cdr expr)
          (type:make-fun (parse-type ret type-env)
                         (loop for param in params
                               collect (parse-type param type-env)))))
       (otherwise
        (let ((alias (find-alias (car expr) type-env))
              (rest (loop for type in (cdr expr)
                          collect (parse-type type type-env))))
          (if alias
              (progn
                (assert (= (length rest) (length (first alias))))
                (type:subst-type
                 (type:make-tysubst (mapcar #'cons (first alias) rest))
                 (second alias)))
              (let ((adt-def (find-adt-def (car expr) type-env)))
                (cond (adt-def
                       (assert (= (type:arity adt-def) (length rest)))
                       (type:make-adt adt-def rest))
                      (t (let ((ph (make-instance 'type:unknown
                                     :form expr
                                     :type-env (lexical-type-env type-env))))
                           (restart-case
                               (error 'unknown-type :name (car expr)
                                                    :placeholder ph)
                             (continue () ph)))))))))))))

(defun reparse-type (unty type-env)
  (check-type unty type:unknown)
  (let ((type-env (rehome-type-env (type:type-env unty) type-env)))
    (type:transform-unknown unty (parse-type (type:expr unty) type-env))))
