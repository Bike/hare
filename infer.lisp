(in-package #:hare)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type inference
;;;

(defclass tenv ()
  (;; An alist (variable . schema)
   (%bindings :initarg :bindings :accessor bindings)))

(defun make-tenv (map) (make-instance 'tenv :bindings map))

(defun subst-tenv (tysubst tenv)
  (make-tenv (mapcar (lambda (pair)
                       (cons (car pair)
                             (subst-schema tysubst (cdr pair))))
                     (bindings tenv))))

(defun lookup-type (key tenv)
  (or (cdr (assoc key (bindings tenv) :test #'type=))
      (error "Unbound: ~s" key)))

(defun restrict-tenv (key tenv)
  (make-tenv (remove key (bindings tenv) :key #'car :test #'type=)))

(defun extend-tenv (key schema tenv)
  (make-tenv (acons key schema (bindings tenv))))

(defun extend-tenv-list (keys schemata tenv)
  (make-tenv (nconc (mapcar #'cons keys schemata) (bindings tenv))))

(defun free-in-tenv (tenv)
  (reduce #'union (bindings tenv)
          :key (lambda (pair) (free-in-schema (cdr pair)))))

;;; HM operation: Return a schema that binds all free variables in type
;;; that are not bound in the environment.
(defun generalize (tenv type)
  (schema type (set-difference (free type) (free-in-tenv tenv)
                               :test #'type=)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unification
;;;

;;; Unify the left type to the right. Return a substitution.
(defgeneric unify/2 (t1 t2)
  (:method ((t1 type) (t2 type))
    (error "Couldn't unify: ~s ~s" t1 t2)))

;;; Given two lists of types, unify corresponding elements.
;;; The lists are assumed to be the same length.
(defun unify-pairwise (typelist1 typelist2)
  (loop with tysubst = (empty-tysubst)
        for t1 in typelist1
        for t2 in typelist2
        do (setf tysubst (compose-tysubst (unify/2 t1 t2) tysubst))
        finally (return tysubst)))

;; Does t1 occur in t2?
(defun occurs (t1 t2)
  (mapnil-type (lambda (ty)
                 (when (type= ty t1)
                   (return-from occurs t)))
               t2))

(defmethod unify/2 ((t1 tvar) (t2 type))
  (cond ((type= t1 t2) nil)
        ((occurs t1 t2)
         (error "Failed occurs check: ~s ~s" t1 t2))
        (t (make-tysubst (list (cons t1 t2))))))
(defmethod unify/2 ((t1 type) (t2 tvar)) (unify/2 t2 t1))

(defmethod unify/2 ((t1 int) (t2 int))
  (if (= (int-type-length t1) (int-type-length t2))
      nil
      (call-next-method)))

(defmethod unify/2 ((t1 pointer) (t2 pointer))
  (unify/2 (pointer-type-underlying t1) (pointer-type-underlying t2)))

(defmethod unify/2 ((t1 fun) (t2 fun))
  (let ((t1params (parameters t1)) (t2params (parameters t2)))
    (if (/= (length t1params) (length t2params))
        (call-next-method)
        (unify-pairwise (cons (fun-return t1) t1params)
                        (cons (fun-return t2) t2params)))))

(defmethod unify/2 ((t1 arrayt) (t2 arrayt))
  (unify/2 (arrayt-element-type t1) (arrayt-element-type t2)))

(defmethod unify/2 ((t1 adt) (t2 adt))
  (let ((t1args (adt-args t1)) (t2args (adt-args t2)))
    (if (and (eq (adt-def t1) (adt-def t2))
             (= (length t1args) (length t2args)))
        (unify-pairwise t1args t2args)
        (call-next-method))))

(defun unify (&rest types)
  (if (null types)
      (error "Cannot unify nothing")
      ;; I think this is fine?
      (loop with tysubst = (empty-tysubst)
            with type1 = (first types)
            for type2 in (rest types)
            do (setf tysubst (compose-tysubst (unify/2 type1 type2) tysubst))
            finally (return tysubst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Onto inference.
;;; The inference functions have the side effect of changing the TYPE slot of
;;; initializers/ASTs. They return one of these things, a combination of
;;; a subst with some mappings from names to types; this should be useful to
;;; modules and stuff.

(defclass inference ()
  ((%tysubst :initarg :tysubst :reader tysubst :type tysubst)
   ;; An alist of (variable . type*)
   (%variable-map :initarg :varmap :reader varmap :type list)
   ;; An alist of (literal . type*)
   (%constant-map :initarg :constmap :reader constmap :type list)))

(defun empty-varmap () nil)
(defun empty-constmap () nil)

(defun make-varmap (alist) alist)
(defun make-constmap (alist) alist)

(defun make-inference (tysubst varmap constmap)
  (make-instance 'inference
    :tysubst tysubst :varmap varmap :constmap constmap))

(defun empty-inference ()
  (make-instance 'inference
    :tysubst (empty-tysubst) :varmap nil :constmap nil))

(defun merge-map/2 (map1 map2)
  (let ((result (copy-alist map1)))
    (loop for (key . rest) in map2
          for pair = (assoc key result :test #'eq)
          if pair
            do (setf (cdr pair) (append rest (cdr pair)))
          else do (push (cons key rest) result))
    result))

(defun subst-map (tysubst map)
  (loop for (key . rest) in map
        collect (cons key
                      ;; We can get duplication by applying a tysubst, for
                      ;; example if two tvars are later unified.
                      (remove-duplicates
                       (loop for ty in rest
                             collect (subst-type tysubst ty))
                       :test #'type=))))

(defun subst-inference (tysubst inference)
  (make-instance 'inference
    :tysubst (compose-tysubst tysubst (tysubst inference))
    :varmap (subst-map tysubst (varmap inference))
    :constmap (subst-map tysubst (constmap inference))))

(defun compose-inferences/2 (inference1 inference2)
  (let ((tysubst (compose-tysubst (tysubst inference1) (tysubst inference2))))
    (make-instance 'inference
      :tysubst tysubst
      :varmap (subst-map tysubst (merge-map/2 (varmap inference1)
                                              (varmap inference2)))
      :constmap (subst-map tysubst (merge-map/2 (constmap inference1)
                                                (constmap inference2))))))

(defun compose-inferences (list)
  (cond ((null list) (empty-inference))
        ((null (rest list)) (first list))
        (t (reduce #'compose-inferences/2 list))))

;;; Apply an inference's subst to its maps.
(defun finalize-inference (inference)
  (let ((tysubst (tysubst inference)))
    (make-instance 'inference
      :tysubst tysubst
      :varmap (subst-map tysubst (varmap inference))
      :constmap (subst-map tysubst (constmap inference)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initializer types
;;; INFER-INITIALIZER-TOPLEVEL is prolly the usual entry into this file.
;;;

(defgeneric infer-initializer (initializer tenv))

(defun infer-initializer-toplevel (initializer tenv)
  (let* ((inference (infer-initializer initializer tenv))
         (tysubst (tysubst inference)))
    (flet ((subst-ast (ast)
             (setf (type ast) (subst-type tysubst (type ast)))))
      (mapnil-initializer (lambda (initializer)
                            (setf (type initializer)
                                  (subst-type tysubst (type initializer)))
                            (when (typep initializer 'lambda-initializer)
                              (mapnil-ast #'subst-ast (body initializer))))
                          initializer))
    (finalize-inference inference)))

(defmethod infer-initializer ((initializer integer-initializer) tenv)
  (declare (ignore tenv))
  ;; Must be an integer type, but we can't express that in the type system.
  (setf (type initializer) (make-tvar '#:integer))
  (empty-inference))

(defmethod infer-initializer ((initializer variable-initializer) tenv)
  (let* ((variable (variable initializer))
         (type (instantiate (lookup-type variable tenv))))
    (setf (type initializer) type)
    (make-instance 'inference
      :tysubst (empty-tysubst) :constmap (empty-constmap)
      :varmap (make-varmap (list (list variable type))))))

(defmethod infer-initializer ((initializer constructor-initializer) tenv)
  (let* ((constructor (constructor initializer))
         (fields (fields initializer)))
    (multiple-value-bind (tapp fieldtys) (instantiate-constructor constructor)
      (setf (type initializer) tapp)
      (let ((finference
              (compose-inferences
               (loop for field in fields
                     collect (infer-initializer field tenv))))
            (usubst (unify-pairwise (mapcar #'type fields) fieldtys)))
        (subst-inference usubst finference)))))

(defmethod infer-initializer ((initializer undef-initializer) tenv)
  (declare (ignore tenv))
  ;; undef can be anything.
  (setf (type initializer) (make-tvar '#:any))
  (empty-inference))

(defmethod infer-initializer ((initializer lambda-initializer) tenv)
  (let* ((params (params initializer)) (body (body initializer))
         (paramtvars (loop for param in params
                           collect (make-tvar (name param))))
         ;; Not polymorphic, as per typed lambda calculus computability
         ;; (Also because what would passing a polymorphic object mean.)
         (paramsc (mapcar #'schema paramtvars))
         ;; FIXME: We need to extend the GLOBAL tenv, here,
         ;; but as-is, if we initialized a local variable with a lambda
         ;; for some reason, inference would see local variable bindings
         ;; even though it shouldn't.
         ;; This could only happen if lambda initializers were allowed
         ;; within functions.
         (new-tenv (extend-tenv-list params paramsc tenv))
         (inference (infer body new-tenv))
         (bodytype (type body))
         (tysubst (tysubst inference)))
    (setf (type initializer)
          (make-fun bodytype (loop for ty in paramtvars
                                   collect (subst-type tysubst ty))))
    inference))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main event
;;; Infer the type of the given initializer or AST.
;;;

(defgeneric infer (ast tenv))

(defmethod infer ((ast reference) tenv)
  (let* ((variable (variable ast))
         (ty (instantiate (lookup-type variable tenv))))
    (setf (type ast) ty)
    (make-inference (empty-tysubst)
                    (make-varmap (list (cons variable ty)))
                    (empty-constmap))))

(defmethod infer ((ast literal) tenv)
  (let* ((initializer (initializer ast))
         (inference (infer-initializer initializer tenv))
         (ty (type initializer)))
    (setf (type ast) ty)
    inference))

(defmethod infer ((ast seq) tenv)
  ;; FIXME: Unify the discarded values with Unit. (and define Unit.)
  (let ((ignored (butlast (asts ast)))
        (final (first (last (asts ast)))))
    (unless (null ignored) (error "whoops not implemented"))
    (prog1 (infer final tenv)
      (setf (type ast) (type final)))))

(defmethod infer ((ast branch) tenv)
  (let ((itest (infer (test ast) tenv))
        (ithen (infer (then ast) tenv))
        (ielse (infer (else ast) tenv))
        (tt (unify (type (test ast)) (make-bool)))
        (tu (unify (type (then ast)) (type (else ast)))))
    (setf (type ast) (type (then ast)))
    (subst-inference tt
                     (subst-inference tu
                                      (compose-inferences
                                       (list itest ithen ielse))))))

(defmethod infer ((ast bind) tenv)
  (let* ((value (value ast))
         (ivalue (infer (value ast) tenv))
         (valt (type value))
         (new-tenv (subst-tenv (tysubst ivalue) tenv))
         #+polymorphic-local
         (valsc (generalize new-tenv valt))
         #-polymorphic-local
         (valsc (schema valt))
         (new-tenv (extend-tenv (variable ast) valsc new-tenv))
         (ibody (infer (body ast) new-tenv)))
    (setf (type ast) (type ibody))
    (compose-inferences/2 ivalue ibody)))

#|
(defmethod infer ((ast initialization) tenv)
  (values (initializer-type (initializer ast) tenv) (empty-subst)))

(defmethod infer ((ast with) tenv)
  ;; Basically like BIND/LET, except we use the initializer's type.
  (let* (;; We ignore the returned subst because we know it's empty:
         ;; the initialization is an initialization, and check the
         ;; infer method above
         (type (infer (initialization ast) tenv))
         ;; This'll force unification with (pointer tvar) as we want.
         ;; (I mean, assuming the body uses the thing.)
         (tptr (make-pointer tvar))
         #+polymorphic-local
         (sc (generalize tenv tptr))
         #-polymorphic-local
         (sc (schema tptr))
         (new-env (extend-tenv (variable ast) sc env)))
    (infer (body ast) new-env)))
|#

(defmethod infer ((ast case) tenv)
  (when (case!p ast) (error "not implemented yet"))
  (multiple-value-bind (tapp cmap) (instantiate-adt-def (adt-def ast))
    (let* ((clauses (clauses ast))
           (value (value ast))
           (ivalue (infer value tenv))
           (vsubst (unify (type value) tapp))
           (iclauses
             (loop for clause in clauses
                   for constructor = (constructor clause)
                   for variables = (variables clause)
                   for fieldtys = (cdr (assoc constructor cmap :test #'eq))
                   ;; Note that case bindings are monomorphic.
                   for fieldscs = (mapcar #'schema fieldtys)
                   for new-tenv = (extend-tenv-list variables fieldscs tenv)
                   collect (infer (body clause) new-tenv)))
           (csubst (apply #'unify (mapcar #'type clauses))))
      (setf (type ast) (type (body (first clauses))))
      (subst-inference
       vsubst
       (subst-inference
        csubst
        (compose-inferences (list* ivalue iclauses)))))))

(defmethod infer ((ast call) tenv)
  (let* ((args (args ast))
         (iargs (loop for arg in args collect (infer arg tenv)))
         (callee (callee ast))
         (icallee (infer callee tenv))
         (rett (make-tvar '#:ret))
         (csubst (unify (type callee)
                        (make-pointer
                         (make-fun rett (mapcar #'type args))))))
    (setf (type ast) rett)
    (subst-inference csubst
                     (compose-inferences (list* icallee iargs)))))
