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
  (let ((info (lookup symbol env)))
    (etypecase info
      (variable-info (make-instance 'reference :variable (variable info)))
      (constant-info
       (make-instance 'literal :initializer (initializer info)))
      (symbol-macro-info
       (convert (funcall (expander info) symbol env) env type-env)))))

(defun convert-cons (head args env type-env)
  (if (symbolp head)
      (let ((info (lookup head env)))
        (etypecase info
          (variable-info ; ordinary function call
           (make-instance 'call
             :callee (variable info)
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
      (make-instance 'call
        :callee (convert head env type-env)
        :args (convertlis args env type-env))))

(defgeneric convert-special (operator rest env type-env))

(defmethod convert-special ((operator (eql 'seq)) rest env type-env)
  (make-instance 'seq :asts (convertlis rest env type-env)))

(defmethod convert-special ((operator (eql 'let)) rest env type-env)
  (destructuring-bind ((varname value) &rest body) rest
    (let* ((lvar (make-variable varname))
           (info (make-instance 'variable-info :variable lvar))
           (new-env (make-env (list varname) (list info) env)))
      (make-instance 'bind
        :var lvar :value (convert value env type-env)
        :body (convertlis body new-env type-env)))))

(defmethod convert-special ((operator (eql 'if)) rest env type-env)
  (destructuring-bind (test then else) rest
    (make-instance 'branch
      :test (convert test env type-env)
      :then (convert then env type-env)
      :else (convert else env type-env))))

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
                 (undef))))
      (make-instance 'with
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

(defun convert-case (head args env type-env)
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

(defmethod convert-special ((operator (eql 'case)) args env type-env)
  (convert-case operator args env type-env))
(defmethod convert-special ((operator (eql 'case!)) args env type-env)
  (convert-case operator args env type-env))

(defmethod convert-special ((operator (eql 'quote)) args env type-env)
  (destructuring-bind (spec) args
    (make-instance 'literal
      ;; FIXME: Should be a global env
      :initializer (parse-literal spec env type-env))))

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
