(in-package #:hare)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; An instance is an initializer with a concrete type, i.e. no free tvars.
;;; Given a module's bindings, we can make instances based on concrete types.
;;; This process is recursive, in that one instance might imply another,
;;; e.g. if we have f = (lambda (x) x) and g = (lambda (x) (f x)), and want to
;;; instantiate g with x's type = (int 32), we'll need to instantiate f
;;; similarly.
;;;
;;; Sometimes there may be free internal tvars, for example due to integer
;;; types. I'm not sure what to do about this right now.
;;;
;;; Here, instance-bindings is an alist (variable initializer type) at the
;;; beginning. The type may contain free tvars, which are unified with the
;;; concrete type we're instantiating with. The alist is modified by pushing
;;; concrete types onto the end as they're added, so we return an alist of
;;; (variable initializer type concrete-type*)
;;;

(defun ensure-instantiation (variable type instance-bindings)
  ;; go
  (let ((entry (assoc variable instance-bindings :test #'eq)))
    (if entry
        (let ((existing (find type (cdddr entry) :test #'eq)))
          (unless existing
            ;; Add a new instantiation.
            (push type (cdddr entry))
            ;; Recur
            (instantiate-initializer
             (second entry) (third entry) type instance-bindings)))
        (error "BUG: Unknown variable during instantiation: ~a" variable))))

;;; Carry out any further instantiations needed to give this initializer
;;; this type. Also check that the type/init combo even makes sense.
(defgeneric instantiate-initializer
    (initializer abstract concrete instance-bindings))

(defmethod instantiate-initializer :before ((initializer initializer)
                                            (abstract type) (concrete type)
                                            instance-bindings)
  ;; san check
  (unless (null (free concrete))
    (error "BUG: Free tvars in type for instantiation: ~a"
           (unparse-type concrete))))

(defmethod instantiate-initializer ((initializer integer-initializer)
                                    (abstract type) (concrete int)
                                    instance-bindings)
  (declare (ignore instance-bindings))
  ;; Just verify it.
  (let ((value (value initializer)) (len (int-type-length concrete)))
    (unless (>= len (integer-length value))
      (error "Integer constant ~a cannot be instantiated as ~a: ~
              type too short"
             value (unparse-type concrete)))))

(defmethod instantiate-initializer ((initializer integer-initializer)
                                    (abstract type) (concrete type)
                                    instance-bindings)
  (declare (ignore instance-bindings))
  (error "Cannot instantiate integer constant ~a as non-integer type ~a"
         (value initializer) (unparse-type concrete)))

(defmethod instantiate-initializer ((initializer undef-initializer)
                                    (abstract type) (concrete type)
                                    instance-bindings)
  (declare (ignore instance-bindings)))

(defmethod instantiate-initializer ((initializer variable-initializer)
                                    (abstract type) (concrete type)
                                    instance-bindings)
  (ensure-instantiation (variable initializer) concrete instance-bindings))

(defmethod instantiate-initializer ((initializer constructor-initializer)
                                    (abstract adt) (concrete adt)
                                    instance-bindings)
  (let ((def (adt-def initializer)))
    (unless (and (eq def (adt-def abstract))
                 (eq def (adt-def concrete)))
      ;: FIXME: better message
      (error "ADT def mismatch while instantiating"))
    (let ((abstract-subst
            (mapcar #'cons (tvars def) (adt-args abstract)))
          (concrete-subst
            (mapcar #'cons (tvars def) (adt-args concrete)))
          (types (elt (members def)
                      (position (constructor initializer)
                                (constructors def)
                                :type #'eq))))
      (loop for type in types
            for field in (fields initializer)
            for abst = (subst-type abstract-subst type)
            for conc = (subst-type concrete-subst type)
            do (instantiate-initializer field abst conc instance-bindings)))))

(defmethod instantiate-initializer ((initializer lambda-initializer)
                                    (abstract fun) (concrete fun)
                                    instance-bindings)
  (let ((subst (unify abstract concrete)))
    (instantiate-ast (body initializer) subst instance-bindings)))

(defmethod instantiate-initializer ((initializer lambda-initializer)
                                    (abstract type) (concrete type)
                                    instance-bindings)
  (error "Cannot instantiate function as non-function type ~a"
         (unparse-type concrete)))

;;; Carry out further instantiations yada yada but with an AST.
;;; Subst describes what type variables are bound to.
;;; If I was smart, I'd use mapnil-ast, but I ain't smart
(defgeneric instantiate-ast (ast subst instance-bindings))

(defun instantiate-asts (asts subst instance-bindings)
  (loop for ast in asts do (instantiate-ast ast subst instance-bindings)))

(defmethod instantiate-ast ((ast seq) subst instance-bindings)
  (instantiate-asts (asts ast) subst instance-bindings))
(defmethod instantiate-ast ((ast call) subst instance-bindings)
  (instantiate-ast (callee ast) subst instance-bindings)
  (instantiate-asts (args ast) subst instance-bindings))
(defmethod instantiate-ast ((ast literal) subst instance-bindings)
  (instantiate-initializer (initializer ast)
                           (subst-type subst (type ast))
                           instance-bindings))
(defmethod instantiate-ast ((ast reference) subst instance-bindings)
  (ensure-instantiation (variable ast)
                        (subst-type subst (type ast))
                        instance-bindings))
(defmethod instantiate-ast ((ast branch) subst instance-bindings)
  (instantiate-ast (test ast) subst instance-bindings)
  (instantiate-ast (then ast) subst instance-bindings)
  (instantiate-ast (else ast) subst instance-bindings))
(defmethod instantiate-ast ((ast bind) subst instance-bindings)
  (instantiate-ast (value ast) subst instance-bindings)
  (instantiate-ast (body ast) subst instance-bindings))
(defmethod instantiate-ast ((ast initialization) subst instance-bindings)
  (instantiate-initializer (initializer ast)
                           (subst-type subst (type ast))
                           instance-bindings))
(defmethod instantiate-ast ((ast with) subst instance-bindings)
  (instantiate-ast (initialization ast) subst instance-bindings)
  (instantiate-ast (body ast) subst instance-bindings))
(defmethod instantiate-ast ((ast case) subst instance-bindings)
  (instantiate-ast (value ast) subst instance-bindings)
  (loop for (_ . sub) in (cases ast)
        do (instantiate-ast sub subst instance-bindings)))

;;; This is stupid. FIXME
(defun initial-module-instance (bindings)
  (loop for (var . init) in bindings collect (list var init)))
