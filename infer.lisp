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
                             (type:subst-schema tysubst (cdr pair))))
                     (bindings tenv))))

(defun lookup-type (key tenv)
  (or (cdr (assoc key (bindings tenv) :test #'eq))
      (error "Unbound: ~s" key)))

(defun restrict-tenv (key tenv)
  (make-tenv (remove key (bindings tenv) :key #'car :test #'type:type=)))

(defun extend-tenv (key schema tenv)
  (make-tenv (acons key schema (bindings tenv))))

(defun extend-tenv-list (keys schemata tenv)
  (make-tenv (nconc (mapcar #'cons keys schemata) (bindings tenv))))

(defun free-in-tenv (tenv)
  (reduce #'union (bindings tenv)
          :key (lambda (pair) (type:free-in-schema (cdr pair)))))

;;; HM operation: Return a schema that binds all free variables in type
;;; that are not bound in the environment.
(defun generalize (tenv type)
  (type:schema type (set-difference (type:free type) (free-in-tenv tenv)
                                    :test #'type:type=)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unification
;;;

;;; Unify the left type to the right. Return a substitution.
(defgeneric unify/2 (t1 t2)
  (:method ((t1 type:type) (t2 type:type))
    (error "Couldn't unify: ~s ~s" t1 t2)))

;;; Given two lists of types, unify corresponding elements.
;;; The lists are assumed to be the same length.
(defun unify-pairwise (typelist1 typelist2)
  (loop with tysubst = (type:empty-tysubst)
        for t1 in typelist1
        for st1 = (type:subst-type tysubst t1)
        for t2 in typelist2
        for st2 = (type:subst-type tysubst t2)
        do (setf tysubst (type:compose-tysubsts (unify/2 st1 st2) tysubst))
        finally (return tysubst)))

;; Does t1 occur in t2?
(defun occurs (t1 t2)
  (type:mapnil-type (lambda (ty)
                      (when (type:type= ty t1)
                        (return-from occurs t)))
                    t2))

(defmethod unify/2 ((t1 type:tvar) (t2 type:type))
  (cond ((type:type= t1 t2) (type:empty-tysubst))
        ((occurs t1 t2)
         (error "Failed occurs check: ~s ~s" t1 t2))
        (t (type:make-tysubst (list (cons t1 t2))))))
(defmethod unify/2 ((t1 type:type) (t2 type:tvar)) (unify/2 t2 t1))

(defmethod unify/2 ((t1 type:int) (t2 type:int))
  (if (= (type:int-type-length t1) (type:int-type-length t2))
      (type:empty-tysubst)
      (call-next-method)))

(defmethod unify/2 ((t1 type:pointer) (t2 type:pointer))
  (unify/2 (type:pointer-type-underlying t1) (type:pointer-type-underlying t2)))

(defmethod unify/2 ((t1 type:fun) (t2 type:fun))
  (let ((t1params (type:parameters t1)) (t2params (type:parameters t2)))
    (if (/= (length t1params) (length t2params))
        (call-next-method)
        (unify-pairwise (cons (type:fun-return t1) t1params)
                        (cons (type:fun-return t2) t2params)))))

(defmethod unify/2 ((t1 type:arrayt) (t2 type:arrayt))
  (unify/2 (type:arrayt-element-type t1) (type:arrayt-element-type t2)))

(defmethod unify/2 ((t1 type:adt) (t2 type:adt))
  (let ((t1args (type:adt-args t1)) (t2args (type:adt-args t2)))
    (if (and (eq (type:adt-def t1) (type:adt-def t2))
             (= (length t1args) (length t2args)))
        (unify-pairwise t1args t2args)
        (call-next-method))))

(defun unify (&rest types)
  (if (null types)
      (error "Cannot unify nothing")
      ;; I think this is fine?
      (loop with tysubst = (type:empty-tysubst)
            with type1 = (first types)
            for stype1 = (type:subst-type tysubst type1)
            for type2 in (rest types)
            for stype2 = (type:subst-type tysubst type2)
            do (setf tysubst
                     (type:compose-tysubsts (unify/2 stype1 stype2) tysubst))
            finally (return tysubst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Onto inference.
;;; The inference functions have the side effect of changing the TYPE slot of
;;; initializers/ASTs. They return one of these things, a combination of
;;; a subst with some mappings from names to types; this should be useful to
;;; modules and stuff.

(defclass inference ()
  ((%tysubst :initarg :tysubst :reader tysubst :type type:tysubst)
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
    :tysubst (type:empty-tysubst) :varmap nil :constmap nil))

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
                             collect (type:subst-type tysubst ty))
                       :test #'type:type=))))

;;; Return a new map with all of the variables removed.
(defun varmap-sans (varmap variables)
  (remove-if (lambda (var) (member var variables :test #'eq)) varmap
             :key #'car))

(defun subst-inference (tysubst inference)
  (make-instance 'inference
    :tysubst (type:compose-tysubsts tysubst (tysubst inference))
    :varmap (subst-map tysubst (varmap inference))
    :constmap (subst-map tysubst (constmap inference))))

(defun compose-inferences/2 (inference1 inference2)
  (let ((tysubst
          (type:compose-tysubsts (tysubst inference1) (tysubst inference2))))
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

;;; Given an inference and some variables, return a new inference with those
;;; variables removed from the varmap.
(defun inference-sans (inference variables)
  (make-instance 'inference
    :tysubst (tysubst inference)
    :varmap (varmap-sans (varmap inference) variables)
    :constmap (constmap inference)))

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
    (ast:mapnil-initializer
     (lambda (thing)
       (setf (ast:type thing) (type:subst-type tysubst (ast:type thing))))
     initializer)
    (finalize-inference inference)))

(defmethod infer-initializer ((initializer ast:integer-initializer) tenv)
  (declare (ignore tenv))
  ;; Must be an integer type, but we can't express that in the type system.
  (setf (ast:type initializer) (type:make-tvar (gensym "INTEGER")))
  (empty-inference))

(defmethod infer-initializer ((initializer ast:variable-initializer) tenv)
  (let* ((variable (ast:variable initializer))
         (type (type:instantiate (lookup-type variable tenv))))
    (setf (ast:type initializer) type)
    (make-instance 'inference
      :tysubst (type:empty-tysubst) :constmap (empty-constmap)
      :varmap (make-varmap (list (list variable type))))))

(defmethod infer-initializer ((initializer ast:constructor-initializer) tenv)
  (let* ((constructor (ast:constructor initializer))
         (fields (ast:fields initializer)))
    (multiple-value-bind (tapp fieldtys)
        (type:instantiate-constructor constructor)
      (setf (ast:type initializer) tapp)
      (let ((finference
              (compose-inferences
               (loop for field in fields
                     collect (infer-initializer field tenv))))
            (usubst (unify-pairwise (mapcar #'ast:type fields) fieldtys)))
        (subst-inference usubst finference)))))

(defmethod infer-initializer ((initializer ast:array-initializer) tenv)
  (let ((elements (ast:elements initializer)))
    (if elements
        (let* ((eleminferences (loop for element in elements
                                     collect (infer-initializer element tenv)))
               (u (apply #'unify (mapcar #'ast:type elements)))
               (elemty (type:subst-type u (ast:type (first elements)))))
          (setf (ast:type initializer) (type:make-arrayt elemty))
          (subst-inference u (compose-inferences eleminferences)))
        (let ((ty (type:make-tvar '#:array)))
          (setf (ast:type initializer) (type:make-arrayt ty))
          (empty-inference)))))

(defmethod infer-initializer ((initializer ast:undef-initializer) tenv)
  (declare (ignore tenv))
  ;; undef can be anything.
  (setf (ast:type initializer) (type:make-tvar '#:any))
  (empty-inference))

(defmethod infer-initializer ((initializer ast:lambda-initializer) tenv)
  (let* ((params (ast:params initializer)) (body (ast:body initializer))
         (paramtvars (loop for param in params
                           collect (type:make-tvar (ast:name param))))
         ;; Not polymorphic, as per typed lambda calculus computability
         ;; (Also because what would passing a polymorphic object mean.)
         (paramsc (mapcar #'type:schema paramtvars))
         ;; FIXME: We need to extend the GLOBAL tenv, here,
         ;; but as-is, if we initialized a local variable with a lambda
         ;; for some reason, inference would see local variable bindings
         ;; even though it shouldn't.
         ;; This could only happen if lambda initializers were allowed
         ;; within functions.
         (new-tenv (extend-tenv-list params paramsc tenv))
         (inference (infer body new-tenv))
         (bodytype (ast:type body))
         (tysubst (tysubst inference)))
    (setf (ast:type initializer)
          (type:make-fun bodytype (loop for ty in paramtvars
                                        collect (type:subst-type tysubst ty))))
    (inference-sans inference params)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main event
;;; Infer the type of the given initializer or AST.
;;;

(defgeneric infer (ast tenv))

(defun infer-list (asts tenv) (loop for ast in asts collect (infer ast tenv)))

(defmethod infer ((ast ast:reference) tenv)
  (let* ((variable (ast:variable ast))
         (ty (type:instantiate (lookup-type variable tenv))))
    (setf (ast:type ast) ty)
    (make-inference (type:empty-tysubst)
                    (make-varmap (list (list variable ty)))
                    (empty-constmap))))

(defmethod infer ((ast ast:literal) tenv)
  (let* ((initializer (ast:initializer ast))
         (inference (infer-initializer initializer tenv))
         (ty (ast:type initializer)))
    (setf (ast:type ast) ty)
    inference))

(defmethod infer ((ast ast:seq) tenv)
  (let* ((ignored (ast:asts ast))
         (final (ast:value ast))
         (inert (type:inert))
         (igninf (infer-list ignored tenv))
         (u (apply #'unify inert (mapcar #'ast:type ignored)))
         (finf (infer final tenv)))
    (setf (ast:type ast) (ast:type final))
    (subst-inference u
                     (compose-inferences (list* finf igninf)))))

(defmethod infer ((ast ast:bind) tenv)
  (let* ((bindings (ast:bindings ast))
         (variables (mapcar #'ast:variable bindings))
         (values (mapcar #'ast:value bindings))
         (ivalues (infer-list values tenv))
         (i (compose-inferences ivalues))
         (new-tenv
           (loop with ntenv = (subst-tenv (tysubst i) tenv)
                 for var in variables
                 for value in values
                 for valt = (ast:type value)
                 ;; the VALUES is only here for indenting the #+#-.
                 for valsc
                   = (values #+polymorphic-local(generalize ntenv valt)
                             #-polymorphic-local(type:schema valt))
                 do (setf ntenv (extend-tenv var valsc ntenv))
                 finally (return ntenv)))
         (body (ast:body ast))
         (ibody (infer body new-tenv)))
    (setf (ast:type ast) (ast:type body))
    (inference-sans (compose-inferences/2 i ibody) variables)))

(defmethod infer ((ast ast:with) tenv)
  (let* ((ninf (infer (ast:nelements ast) tenv))
         (var (ast:variable ast))
         (ty (type:make-tvar (symbol-name (ast:name var))))
         (pty (type:make-pointer ty))
         (nenv (extend-tenv var (type:schema pty) tenv))
         (binf (infer (ast:body ast) nenv)))
    (setf (ast:type ast) (ast:type (ast:body ast)))
    (compose-inferences/2 ninf binf)))

(defmethod infer ((ast ast:initialize) tenv)
  (let* ((pinf (infer (ast:value ast) tenv))
         (iinf (infer-initializer (ast:initializer ast) tenv))
         (ty (type:make-tvar)) (pty (type:make-pointer ty))
         (pu (unify (ast:type (ast:value ast)) pty))
         (iu (unify (ast:type (ast:initializer ast)) ty)))
    (setf (ast:type ast) (type:inert))
    (subst-inference pu (subst-inference iu (compose-inferences/2 pinf iinf)))))

(defparameter *primitive-types*
  (list
   ;; load a value from a pointer
   (cons '! (lambda ()
                   (let ((ty (type:make-tvar)))
                     (values ty (list (type:make-pointer ty))))))
   ;; store a value to a pointer
   (cons 'set! (lambda ()
                 (let ((ty (type:make-tvar)))
                   (values (type:inert)
                           (list (type:make-pointer ty) ty)))))
   ;; return a pointer to an element of an array
   (cons 'ref (lambda ()
                (let ((vty (type:make-tvar '#:element))
                      (ity (type:make-int 64)))
                  (values (type:make-pointer vty)
                          (list (type:make-pointer (type:make-arrayt vty))
                                ity)))))
   ;; return a subarray
   (cons 'aref (lambda ()
                 (let ((aty (type:make-pointer
                             (type:make-arrayt (type:make-tvar '#:element))))
                       (ity (type:make-int 64)))
                   (values aty (list aty ity)))))
   ;; Cast a pointer to object into a pointer to bytearray
   (cons 'bytescast (lambda ()
                      (let ((oty (type:make-pointer (type:make-tvar '#:object)))
                            (bytearray (type:make-pointer
                                        (type:make-arrayt
                                         ;; FIXME: avoid magic number
                                         (type:make-int 8)))))
                        (values bytearray (list oty)))))
   ;; Cast a pointer to bytearray to a pointer to object
   (cons 'castbytes (lambda ()
                      (let ((oty (type:make-pointer (type:make-tvar '#:object)))
                            (bytearray (type:make-pointer
                                        (type:make-arrayt (type:make-int 8)))))
                        (values oty (list bytearray)))))))

(defmethod infer ((ast ast:primitive) tenv)
  ;; basically a slightly dumber version of call ASTs.
  (let* ((name (ast:name ast)) (args (ast:args ast))
         (pair (assoc name *primitive-types*))
         (thunk (or (cdr pair) (error "Unknown primitive ~a" name))))
    (multiple-value-bind (rett argts) (funcall thunk)
      (let* ((iargs (infer-list args tenv))
             (au (unify-pairwise argts (mapcar #'ast:type args)))
             (rett (type:subst-type au rett)))
        (setf (ast:type ast) rett)
        (subst-inference au (compose-inferences iargs))))))

(defmethod infer ((ast ast:case) tenv)
  (multiple-value-bind (tapp cmap) (type:instantiate-adt-def (type:adt-def ast))
    (let* ((!p (ast:case!p ast))
           (rtapp (if !p (type:make-pointer tapp) tapp))
           (clauses (ast:clauses ast))
           (value (ast:value ast))
           (ivalue (infer value tenv))
           (vsubst (unify (ast:type value) rtapp))
           (iclauses
             (loop for clause in clauses
                   for constructor = (ast:constructor clause)
                   for variables = (ast:variables clause)
                   for fieldtys = (cdr (assoc constructor cmap :test #'eq))
                   for rfieldtys = (if !p
                                       (mapcar #'type:make-pointer fieldtys)
                                       fieldtys)
                   ;; Note that case bindings are monomorphic.
                   for fieldscs = (mapcar #'type:schema rfieldtys)
                   for new-tenv = (extend-tenv-list variables fieldscs tenv)
                   for prei = (infer (ast:body clause) new-tenv)
                   collect (inference-sans prei variables)))
           (bodies (mapcar #'ast:body clauses))
           (csubst (apply #'unify (mapcar #'ast:type bodies))))
      (setf (ast:type ast) (ast:type (ast:body (first clauses))))
      (subst-inference
       vsubst
       (subst-inference
        csubst
        (compose-inferences (list* ivalue iclauses)))))))

(defmethod infer ((ast ast:construct) tenv)
  (let* ((constructor (ast:constructor ast))
         (args (ast:args ast))
         (iargs (infer-list args tenv)))
    (multiple-value-bind (adt consfields)
        (type:instantiate-constructor constructor)
      (let ((s (unify-pairwise consfields (mapcar #'ast:type args))))
        (setf (ast:type ast) (type:subst-type s adt))
        (subst-inference s (compose-inferences iargs))))))

(defmethod infer ((ast ast:call) tenv)
  (let* ((args (ast:args ast))
         (iargs (infer-list args tenv))
         (callee (ast:callee ast))
         (icallee (infer callee tenv))
         (retname (if (typep callee 'ast:reference)
                      (concatenate 'string
                                   (symbol-name
                                    (ast:name (ast:variable callee)))
                                   "-RET")
                      "RET"))
         (rett (type:make-tvar (gensym retname)))
         (csubst (unify (ast:type callee)
                        (type:make-pointer
                         (type:make-fun rett (mapcar #'ast:type args))))))
    (setf (ast:type ast) rett)
    (subst-inference csubst
                     (compose-inferences (list* icallee iargs)))))
