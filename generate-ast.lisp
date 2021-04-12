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

(defun convert-seq (list env type-env)
  (when (null list) (error "empty seq disallowed"))
  (let ((cl (convertlis list env type-env)))
    (make-instance 'ast:seq :asts (butlast cl) :value (first (last cl)))))

(defun convert-symbol (symbol env type-env)
  (let ((info (lookup symbol env)))
    (etypecase info
      (variable-info (make-instance 'ast:reference :variable (variable info)))
      (constant-info
       (make-instance 'ast:literal :initializer (initializer info)))
      (symbol-macro-info
       (convert (funcall (expander info) symbol env) env type-env)))))

(defun convert-cons (head args env type-env)
  (if (symbolp head)
      (let ((info (lookup head env)))
        (etypecase info
          (variable-info ; ordinary function call
           (make-instance 'ast:call
             :callee (make-instance 'ast:reference :variable (variable info))
             :args (convertlis args env type-env)))
          (symbol-macro-info
           (convert-cons (funcall (expander info) head env)
                         args env type-env))
          (macro-info
           (convert (funcall (expander info) (cons head args) env)
                    env type-env))
          (special-operator-info
           (convert-special head args env type-env))))
      ;; ordinary function call with complex evaluation of the function
      (make-instance 'ast:call
        :callee (convert head env type-env)
        :args (convertlis args env type-env))))

(defgeneric convert-special (operator rest env type-env))

(defmethod convert-special ((operator (eql 'seq)) rest env type-env)
  (convert-seq rest env type-env))

(defmethod convert-special ((operator (eql 'let)) rest env type-env)
  (destructuring-bind ((varname value) &rest body) rest
    (let* ((lvar (ast:make-variable varname))
           (info (make-instance 'variable-info :variable lvar))
           (new-env (make-env (list varname) (list info) env)))
      (make-instance 'ast:bind
        :variable lvar :value (convert value env type-env)
        :body (convert-seq body new-env type-env)))))

#+(or)
(defmethod convert-special ((operator (eql 'with)) rest env type-env)
  (destructuring-bind ((var &optional (initializer nil initializerp))
                       &rest body)
      rest
    (let* ((lvar (make-variable var))
           (info (make-instance 'variable-info :variable lvar))
           (initializer
             (if initializerp
                 ;; FIXME: Should be a global env probably
                 (parse-initializer initializer env type-env)
                 (ast:undef))))
      (make-instance 'ast:with
        :var lvar
        :initialization (make-instance 'initialization
                          :initializer initializer)
        :body (convertlis
               body (make-env (list var) (list info) env) type-env)))))
#+(or)
(defmethod convert-special ((operator (eql 'with-array)) rest env type-env)
  (destructuring-bind ((var len) &rest body) rest
    (let ((lvar (make-variable var)))
      (make-instance 'with
        :var (make-variable var) :len (convert len env type-env)
        :body (convert body (acons var lvar env) type-env)))))

(defun convert-clause (constructor-name varnames bodyforms env type-env)
  (let* ((constructor (find-constructor constructor-name type-env))
         (variables (loop for varname in varnames
                          collect (ast:make-variable varname)))
         (var-infos (loop for variable in variables
                          collect (make-instance 'variable-info
                                    :variable variable)))
         (body-env (make-env varnames var-infos env))
         (body (convert-seq bodyforms body-env type-env)))
    (make-instance 'ast:case-clause
      :constructor constructor :variables variables :body body)))

(defun convert-case (head args env type-env)
  (destructuring-bind (value &rest clauses) args
    (when (null clauses)
      (error "Empty case"))
    (let* ((clauses
             (loop for ((cname . vars) . body) in clauses
                   collect (convert-clause cname vars body env type-env)))
           (adt-def (type:adt-def (ast:constructor (first clauses)))))
      (assert (loop for clause in (rest clauses)
                    always (eq adt-def
                               (type:adt-def (ast:constructor clause)))))
      (make-instance 'ast:case
        :value (convert value env type-env)
        :clauses clauses
        :case!p (eq head 'case!)
        :adt-def adt-def))))

(defmethod convert-special ((operator (eql 'case)) args env type-env)
  (convert-case operator args env type-env))
(defmethod convert-special ((operator (eql 'case!)) args env type-env)
  (convert-case operator args env type-env))

(defmethod convert-special ((operator (eql 'cons)) args env type-env)
  (make-instance 'ast:construct
    :constructor (find-constructor (first args) type-env)
    :args (convertlis (rest args) env type-env)))

(defmethod convert-special ((operator (eql 'quote)) args env type-env)
  (destructuring-bind (spec) args
    (make-instance 'ast:literal
      ;; FIXME: Should be a global env
      :initializer (parse-literal spec env type-env))))

(defun convert-literal (form env type-env)
  (make-instance 'ast:literal
    :initializer (parse-literal form env type-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsing literals and initializers
;;;

(defun parse-literal (literal env type-env)
  (etypecase literal
    ((integer 0) (make-instance 'ast:integer-initializer :value literal))
    ((or (cons (member array arrayn bytes lambda)) (eql undef))
     (error "Found initializer in literal context: ~a" literal))
    (symbol (lookup literal env))
    (cons ; constructor
     (let* ((cname (car literal)) (fields (cdr literal))
            (constructor (find-constructor cname type-env)))
       (make-instance 'ast:constructor-initializer
         :constructor constructor
         :fields (loop for field in fields
                       collect (parse-literal field env type-env)))))))

(defun parse-initializer (initializer env type-env)
  (etypecase initializer
    ((integer 0) (make-instance 'ast:integer-initializer :value initializer))
    ((eql undef) (ast:undef))
    (symbol
     (make-instance 'ast:variable-initializer
       :variable (lookup initializer env)))
    ((cons (eql lambda))
     (parse-lambda (cadr initializer) (cddr initializer)
                   env type-env))
    ((cons (eql array))
     (parse-array (rest initializer) env type-env))
    ((cons (member arrayn bytes))
     (error "Not implemented yet: ~a" (car initializer)))
    (cons ; constructor
     (let* ((cname (car initializer)) (fields (cdr initializer))
            (constructor (find-constructor cname type-env)))
       (make-instance 'ast:constructor-initializer
         :constructor constructor
         :fields (loop for field in fields
                       collect (parse-initializer field env type-env)))))))

(defun parse-lambda (params forms env type-env)
  (let* ((vars (mapcar #'ast:make-variable params))
         (infos (loop for var in vars
                      collect (make-instance 'variable-info :variable var)))
         (env (make-env params infos env)))
    (make-instance 'ast:lambda-initializer
      :params vars
      :body (convert-seq forms env type-env))))

(defun parse-array (elementfs env type-env)
  (make-instance 'ast:array-initializer
    :elements (loop for elementf in elementfs
                    collect (parse-initializer elementf env type-env))))
