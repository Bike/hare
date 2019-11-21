(in-package #:hare)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hare but with manifest typing.
;;; cuz i'm gettin a little sick of HM
;;; Basically just parsing but the types are explicit enough to be put in the
;;; ast type slot and etc right off.
;;; This has some sorta weird consequences. Literal integers and quoted
;;; constructions aren't allowed and instead there's a CONSTANT operator.
;;;

;;; adt parsing is identical

(defun mparse-form (form env adt-env)
  (flet ((mparse-seq (list env)
           (let* ((asts (loop for form in list
                              collect (mparse-form form env adt-env)))
                  (type (type (first (last asts)))))
             (make-instance 'seq :asts asts :type type))))
    (etypecase form
      (symbol
       (let* ((pair (assoc form env))
              (thing (cadr pair)) (type (cddr pair)))
         (etypecase thing
           (variable (make-instance 'reference :variable thing :type type))
           (initializer (make-instance 'literal
                          :initializer thing :type type)))))
      (cons
       (let ((head (car form)) (args (cdr form)))
         (cl:case head
           ((seq) (mparse-seq args env))
           ((let)
            (destructuring-bind ((var value) &rest body) args
              (let* ((lvar (make-variable var))
                     (value (mparse-form value env adt-env))
                     (new-env (acons var (cons lvar (type value)) env))
                     (body (mparse-seq body new-env)))
                (make-instance 'bind :var lvar :value value :body body))))
           ((if)
            (destructuring-bind (test then else) args
              (let ((test (mparse-form test env adt-env))
                    (then (mparse-form then env adt-env))
                    (else (mparse-form else env adt-env)))
                ;; type equality is EQ due to caching
                (unless (eq (type then) (type else))
                  (error "Type mismatch: IF branches have distinct types ~
                          ~a and ~a"
                         (unparse-type (type then))
                         (unparse-type (type else))))
                (make-instance 'branch
                  :test test :then then :else else :type (type then)))))
           ((with)
            (destructuring-bind ((var type
                                  &optional (initializer nil initializerp))
                                 &rest body)
                args
              (let ((lvar (make-variable var))
                    (type (parse-type type nil adt-env))
                    (initializer
                      (if initializerp
                          (mparse-initializer initializer env adt-env)
                          (undef))))
                (make-instance 'with
                  :var lvar
                  :initialization (make-instance 'initialization
                                    :initializer initializer :type type)
                  :body (mparse-seq body
                                    (acons var (cons lvar (make-pointer type))
                                           env))))))
           ((case case!)
            (destructuring-bind (value &rest cases) args
              (multiple-value-bind (def cases) (case-adt-def cases adt-env)
                (let* ((value (mparse-form value env adt-env))
                       (vtype (type value)))
                  (unless (eq (adt-def vtype) def)
                    (error "Type mismatch: Case value does not match clauses"))
                  (let ((cases
                          (loop for ((constructor . vars) . body) in cases
                                for types in (members def)
                                for lvars = (mapcar #'make-variable vars)
                                for semi-env = (loop for var in vars
                                                     for type in types
                                                     for lvar in lvars
                                                     collect (cons var (cons lvar type)))
                                for new-env = (nconc semi-env env)
                                collect (cons (cons constructor lvars)
                                              (mparse-seq body new-env)))))
                    (make-instance 'case :value value :cases cases :adt-def def
                                         :case!p (eq head 'case!)))))))
           ((constant)
            (destructuring-bind (type spec) args
              (make-instance 'literal
                :initializer (mparse-literal spec env adt-env)
                :type (parse-type type nil adt-env))))
           (otherwise ; call
            (let* ((callee (mparse-form head env adt-env))
                   (calleet (type callee))
                   (args (loop for form in args
                               collect (mparse-form form env adt-env))))
              (unless (typep calleet 'pointer)
                (error "Type mismatch: Can't call non-pointer ~a"
                       (unparse-type calleet)))
              (let ((ftype (pointer-type-underlying calleet))
                    (argtypes (mapcar #'type args)))
                (unless (typep ftype 'fun)
                  (error "Type mismatch: Can't call pointer to non-function ~a"
                         (unparse-type calleet)))
                (unless (every #'eq (parameters ftype) argtypes)
                  (error "Type mismatch: Bad call. Expected ~a got ~a"
                         (mapcar #'unparse-type (parameters ftype))
                         (mapcar #'unparse-type argtypes)))
                (make-instance 'call
                               :callee callee :args args
                               :type (fun-return ftype)))))))))))

(defun mparse-literal (literal env adt-env)
  (etypecase literal
    ((integer 0) (make-instance 'integer-initializer :value literal))
    ((or (cons (member array arrayn bytes lambda)) (eql undef))
     (error "Found initializer in literal context: ~a" literal))
    (symbol (let ((pair (assoc literal env)))
              (when (null pair) (error "Unbound: ~a" literal))
              (car pair)))
    (cons
     (let* ((constructor (car literal)) (fields (cdr literal))
            (def (find-adt-def constructor adt-env)))
       (make-instance 'constructor-initializer
         :def def :constructor constructor
         :fields (loop for field in fields
                       collect (parse-literal field env adt-env)))))))

(defun mparse-initializer (initializer env adt-env)
  (etypecase initializer
    ((integer 0) (make-instance 'integer-initializer :value initializer))
    ((eql undef) (undef))
    (symbol (let ((pair (assoc initializer env)))
              (when (null pair) (error "Unbound: ~a" initializer))
              (cadr pair)))
    ((cons (eql lambda))
     (mparse-lambda (cadr initializer) (cddr initializer)
                    env adt-env))
    ((cons (member array arrayn bytes))
     (error "Not implemented yet: ~a" (car initializer)))
    (cons ; constructor
     (let* ((constructor (car initializer)) (fields (cdr initializer))
            (def (find-adt-def-from-constructor constructor adt-env)))
       (make-instance 'constructor-initializer
         :def def :constructor constructor
         :fields (loop for field in fields
                       collect (parse-initializer field env adt-env)))))))

(defun mparse-lambda (params forms env adt-env)
  (let* ((pvars (loop for (param _) in params
                      collect (make-variable param)))
         (semi-env (loop for (param type) in params
                         for pvar in pvars
                         for ptype = (parse-type type nil adt-env)
                         collect (cons param (cons pvar ptype))))
         (new-env (nconc semi-env env)))
    (make-instance 'lambda-initializer
      :params pvars
      :body (mparse-form `(seq ,@forms) new-env adt-env))))

(defun manifest-bindings (defvars env adt-env)
  (loop for (def name type . rest) in defvars
        for pair = (assoc name env)
        for var = (or (cadr pair) (error "BUG: Unknown name ~a" name))
        for init = (if rest (first rest) 'undef)
        for pinit = (mparse-initializer init env adt-env)
        collect (list var (cddr pair) pinit)))

(defun manifest-initial-env (defvars defconstants adt-env)
  (unless (null defconstants)
    (error "not implemented yet"))
  (let ((env nil))
    (loop for (def name type) in defvars
          for var = (make-variable name)
          for ptype = (make-pointer (parse-type type nil adt-env))
          do (push (cons name (cons var ptype)) env))
    env))

(defun mparse-module (forms)
  (multiple-value-bind (defadts defvars defconstants)
      (divide-toplevel-forms forms)
    (let* ((adt-env (parse-defadts defadts))
           (initial (manifest-initial-env defvars defconstants adt-env)))
      (manifest-bindings defvars initial adt-env))))
