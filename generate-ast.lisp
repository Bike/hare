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

(defun conversion-missing-name (name form env)
  (let ((ast (make-instance 'ast:unknown
               :expr form :env (lexical-environment env))))
    (restart-case
        (error 'unknown-variable :name name :placeholder ast)
      (continue () ast))))

(defun convert-symbol (symbol env type-env)
  (let ((info (lookup symbol env)))
    (etypecase info
      (variable-info (make-instance 'ast:reference :variable (variable info)))
      (constant-info
       (make-instance 'ast:literal :initializer (initializer info)))
      (symbol-macro-info
       (convert (funcall (expander info) symbol env) env type-env))
      (null (conversion-missing-name symbol symbol env)))))

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
           (convert-special head args env type-env))
          (null (conversion-missing-name head (cons head args) env))))
      ;; ordinary function call with complex evaluation of the function
      (make-instance 'ast:call
        :callee (convert head env type-env)
        :args (convertlis args env type-env))))

(defgeneric convert-special (operator rest env type-env))

(defmethod convert-special ((operator (eql 'seq)) rest env type-env)
  (convert-seq rest env type-env))

(defmethod convert-special ((operator (eql 'let)) rest env type-env)
  (destructuring-bind (bindings &rest body) rest
    (multiple-value-bind (bindings varnames infos)
        (loop for (varname value) in bindings
              for lvar = (ast:make-variable varname)
              for info = (make-instance 'variable-info :variable lvar)
              for cval = (convert value env type-env)
              for binding = (make-instance 'ast:binding
                              :variable lvar :value cval)
              collect varname into varnames
              collect info into infos
              collect binding into bindings
              finally (return (values bindings varnames infos)))
      (let ((new-env (make-env varnames infos env)))
        (make-instance 'ast:bind
          :bindings bindings
          :body (convert-seq body new-env type-env))))))

(defmethod convert-special ((operator (eql 'with)) rest env type-env)
  (destructuring-bind ((var nelements) &rest body)
      rest
    (let* ((lvar (ast:make-variable var))
           (info (make-instance 'variable-info :variable lvar))
           (nelements (convert nelements env type-env)))
      (make-instance 'ast:with
        :variable lvar :nelements nelements
        :body (convert-seq
               body (make-env (list var) (list info) env) type-env)))))

(defmethod convert-special ((operator (eql 'initialize)) rest env type-env)
  (destructuring-bind (ptr init) rest
    (make-instance 'ast:initialize
      :value (convert ptr env type-env)
      :initializer (parse-initializer init env type-env))))

(defun convert-clause (constructor-name varnames bodyforms env type-env)
  (let* ((constructor
           (or (find-constructor constructor-name type-env)
               (return-from convert-clause nil)))
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
                   for clause = (convert-clause cname vars body env type-env)
                   ;; FIXME: Demanding that the whole form be reparsed is not
                   ;; really necessary - the subforms are totally parseable.
                   ;; Better would be to make clauses with unknown constructors
                   ;; that can be filled in later.
                   when clause collect clause
                     else do (let ((ast (make-instance 'ast:unknown
                                          :form (cons head args)
                                          :env (lexical-environment env))))
                               (restart-case
                                   (error 'unknown-constructor
                                          :name cname :placeholder ast)
                                 (continue ()
                                   (return-from convert-case ast))))))
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

(defmethod convert-special ((operator (eql 'primitive)) args env type-env)
  (destructuring-bind (name &rest args) args
    (make-instance 'ast:primitive
      :name name :args (convertlis args env type-env))))

(defmethod convert-special ((operator (eql 'quote)) args env type-env)
  (destructuring-bind (spec) args
    (make-instance 'ast:literal
      :initializer (parse-literal spec (global-environment env) type-env))))

(defun convert-literal (form env type-env)
  (make-instance 'ast:literal
    :initializer (parse-literal form (global-environment env) type-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Having another go at ASTs we gave up on (because new stuff has been defined)
;;;

(defun reparse-ast (unknown env type-env)
  (check-type unknown ast:unknown)
  (let ((env (rehome-environment (ast:environment unknown) env)))
    (ast:transform-unknown unknown (convert (ast:expr unknown) env type-env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsing literals and initializers
;;;

(defun parse-literal (literal env type-env)
  (etypecase literal
    ((integer 0) (make-instance 'ast:integer-initializer :value literal))
    ((or (cons (member array arrayn bytes lambda)) (eql undef))
     (error "Found initializer in literal context: ~a" literal))
    #+(or)
    (symbol) ; actually not sure about semantics here yet
    (cons ; constructor
     (let* ((cname (car literal)) (fields (cdr literal))
            (constructor (find-constructor cname type-env)))
       (if constructor
           (make-instance 'ast:constructor-initializer
             :constructor constructor
             :fields (loop for field in fields
                           collect (parse-literal field env type-env)))
           (let ((i (make-instance 'ast:unknown-initializer
                      :form literal :env (lexical-environment env))))
             (restart-case
                 (error 'unknown-constructor :name cname :placeholder i)
               (continue () i))))))))

(defun parse-initializer (initializer env type-env)
  (etypecase initializer
    ((integer 0) (make-instance 'ast:integer-initializer :value initializer))
    ((eql undef) (ast:undef))
    #+(or)
    (symbol
     (make-instance 'ast:variable-initializer
       :variable (lookup initializer env)))
    ((cons (eql lambda))
     (parse-lambda (cadr initializer) (cddr initializer)
                   env type-env))
    ((cons (eql array))
     (parse-array (rest initializer) env type-env))
    ((cons (eql vla))
     (make-instance 'ast:vla-initializer
       :nelements (convert (second initializer) env type-env)))
    (cons ; constructor
     (let* ((cname (car initializer)) (fields (cdr initializer))
            (constructor (find-constructor cname type-env)))
       (if constructor
           (make-instance 'ast:constructor-initializer
             :constructor constructor
             :fields (loop for field in fields
                           collect (parse-initializer field env type-env)))
           (let ((i (make-instance 'ast:unknown-initializer
                      :form initializer :env (lexical-environment env))))
             (restart-case
                 (error 'unknown-constructor :name cname :placeholder i)
               (continue () i))))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Having another go at initializers
;;; FIXME: We don't have a separate reparse-literal, so we could end up with
;;; initializers where literals are needed, in badly formed code
;;;

(defun reparse-initializer (uninit env type-env)
  (check-type uninit ast:unknown-initializer)
  (let ((env (rehome-environment (ast:environment uninit) env)))
    (ast:transform-unknown-initializer
     uninit
     (parse-initializer (ast:expr uninit) env type-env))))
