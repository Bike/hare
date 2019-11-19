(in-package #:hare)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type inference
;;; A type environment is an alist (variable . schema)
;;; A substitution is an alist (tvar . type)
;;;

(defun subst-tenv (subst tenv)
  (mapcar (lambda (pair)
            (cons (car pair)
                  (subst-schema subst (cdr pair))))
          tenv))

(defun lookup-type (key tenv)
  (or (cdr (assoc key tenv :test #'eq))
      (error "Unbound: ~s" key)))

(defun restrict-tenv (key tenv)
  (remove key tenv :key #'car :test #'eq))

(defun extend-tenv (key schema tenv)
  (acons key schema (restrict-tenv key tenv)))

(defun restrict-tenv-list (keys tenv)
  (remove-if (lambda (key) (find key keys :test #'eq))
             tenv
             :key #'car))

(defun extend-tenv-list (keys schemata tenv)
  (nconc (mapcar #'cons keys schemata)
         (restrict-tenv-list keys tenv)))

(defun free-in-tenv (tenv)
  (reduce #'union tenv :key (lambda (pair) (free-in-schema (cdr pair)))))

;;; HM operation: Return a schema that binds all free variables in type
;;; that are not bound in the environment.
(defun generalize (tenv type)
  (schema type (set-difference (free type) (free-in-tenv tenv)
                               :test #'eq)))

;;;;;;;;;;;;;;;;;;;;
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
  (loop with subst = (empty-subst)
        for t1 in typelist1
        for t2 in typelist2
        do (setf subst (compose-subst (unify/2 t1 t2) subst))
        finally (return subst)))

;; Does t1 occur in t2?
(defun occurs (t1 t2)
  (mapnil-type (lambda (ty)
                 (when (eq ty t1)
                   (return-from occurs t)))
               t2))

(defmethod unify/2 ((t1 tvar) (t2 type))
  (cond ((eq t1 t2) nil)
        ((occurs t1 t2)
         (error "Failed occurs check: ~s ~s" t1 t2))
        (t (list (cons t1 t2)))))
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
      (loop with subst = (empty-subst)
            with type1 = (first types)
            for type2 in (rest types)
            do (setf subst (compose-subst (unify/2 type1 type2) subst))
            finally (return subst))))

;;;;;;;;;;;;;;;;;;;;

(defgeneric infer (ast tenv))

(defmethod infer ((ast reference) tenv)
  (values (lookup-type (variable ast) env) (empty-subst)))
(defmethod infer ((ast seq) tenv)
  ;; FIXME: Unify the discarded values with Unit. (and define Unit.)
  (let ((ignored (butlast (asts ast)))
        (final (first (last (asts ast)))))
    (unless (null ignored) (error "whoops not implemented"))
    (infer final tenv)))
(defmethod infer ((ast branch) tenv)
  (multiple-value-bind (testt testsubst)
      (infer (test ast) tenv)
    (multiple-value-bind (thent thensubst)
        (infer (then ast) tenv)
      (multiple-value-bind (elset elsesubst)
          (infer (else ast) tenv)
        (let ((testsubst2 (unify testt (make-bool)))
              (resultsubst (unify thent elset)))
          (values (subst-type resultsubst thent)
                  (compose-substs
                   resultsubst testsubst2
                   elsesubst thensubst testsubst)))))))
(defmethod infer ((ast bind) tenv)
  (multiple-value-bind (valt valsubst) (infer (value ast) tenv)
    (let* ((new-env (subst-tenv valsubst tenv))
           (valsc (generalize new-env valt)))
      (multiple-value-bind (bodyt bodysubst)
          (infer (body ast) (extend-tenv (var ast) valsc new-env))
        (values bodyt (compose-subst valsubst bodysubst))))))
(defmethod infer ((ast with) tenv)
  ;; Basically like BIND/LET, except we use the initializer's type.
  (let* ((type (initializer-type (initializer ast)))
         ;; This'll force unification with (pointer tvar) as we want.
         ;; (I mean, assuming the body uses the thing.)
         (tptr (make-pointer tvar))
         ;; WITH is polymorphic.
         (sc (generalize tenv tptr))
         (new-env (extend-tenv (variable ast) sc env)))
    (infer (body ast) new-env)))
(defmethod infer ((ast case) tenv)
  ;; I'm pretty unsure about a lot of this.
  ;; Particularly, which inferences need to keep up previous envs,
  ;; when to do substitutions, etc...
  (when (case!p ast) (error "not implemented yet"))
  (let* ((def (adt-def ast))
         (constructors (constructors def)))
    (multiple-value-bind (members adt)
        ;; Make a fresh type as if we were instantiating a schema
        (instantiate-adt-def def)
      (multiple-value-bind (valt valsubst1) (infer (value ast) tenv)
        (let* (;; Unify the value with the adt.
               (valsubst2 (unify valt adt))
               (valsubst (compose-subst valsubst2 valsubst1))
               ;; Substitute
               (tenv (subst-tenv valsubst tenv)))
          ;; Now infer each clause separately.
          (loop with case-types = nil
                with ret-subst = valsubst ; subst we'll return
                for member in members
                for schemata = (mapcar #'schema member)
                for constructor in constructors
                for ((case-cons . variables) . ast) in (cases ast)
                for new-tenv = (extend-tenv-list variables schemata tenv)
                do ;; Make sure everything's in the right order.
                   ;; (It should be made so in parse-form.)
                   (assert (eq case-cons constructor))
                   ;; Infer
                   (multiple-value-bind (type subst)
                       (infer ast new-tenv)
                     (push type case-types)
                     (setf ret-subst (compose-subst subst ret-subst)))
                finally
                   (return
                     ;; Unify all the case types, and return the accumulated
                     ;; subst.
                     (values (apply #'unify case-types) ret-subst))))))))
(defmethod infer ((ast call) tenv)
  (multiple-value-bind (funt funsubst) (infer (callee ast) tenv)
    (let ((tenv (subst-tenv funsubst tenv)))
      (multiple-value-bind (argtypes argsubst)
          (loop with types = nil
                with subst = funsubst
                for arg in (args ast)
                do (multiple-value-bind (argty argsubst)
                       (infer arg tenv)
                     (push argty types)
                     (setf subst
                           (compose-subst argsubst subst)))
                finally (return (values (nreverse types) subst)))
        (let* (;; new variable for the return type
               (rett (make-tvar))
               ;; u-ni-fy
               (callsubst (unify funt (make-pointer
                                       (make-fun rett argtypes)))))
          (values (subst-type callsubst rett)
                  (compose-subst callsubst argsubst)))))))
