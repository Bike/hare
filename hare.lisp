(defpackage #:hare
  (:use #:cl)
  (:shadow #:type #:case))

(in-package #:hare)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stuff for the low level language (Hare)
;;;

#| Language description:
"close to the machine", like C, but with ML style type inference.
A program consists of zero or more
 * function definitions:
     (defun name (parameter*) body) defines a function bla bla bla.
     This function may be polymorphic.
     If a param is (param type) instead, this definition is part of
      an ad hoc setup. Otherwise it's parametric.
 * type definitions
     (defadt name (param*) (constructor type*)*)
     Define a new type schema. Works like Haskell.
     Note that values, types, and constructors are in distinct
      namespaces.
     (defalias name (param*) type) defines a transparent alias.
     It's not haskell newtype, just a new name. (newtype is not necessary
      because we evaluate eagerly.)
     Aliases cannot be recursive, since it's basically a macro mechanism.
     If there are no params, (name) and name both work -
      for both defadt and defalias.
 * variable definitions
     (defvar name [literal]) - name will be a pointer. not implemented yet
 * constant definitions
     (defconstant name value)
     Value must be either an integer literal or uh haven't worked out
      constructors yet, ask me later.

There is a static, strong, inference based type system.
Due to memory concerns, types are divided into "sized" and "unsized"
types. Sized types are allocated with WITH, deconstructed with
CASE, and can have pointers to them dereferenced. Unsized types are
allocated with WITH-ARRAY, deconstructed with CASE!, cannot have
pointers to them dereferenced, and cannot be bound to variables or passed
to functions.

New types, called ADTs for Algebraic Data Types, are defined with defadt.
Specifically, defadt defines a type schema, and then you fill it out
with args at some later point.
An ADT can have at most one unsized type argument.

Sized types consist of:
 * (int n) is an integer of n bits. Un/signed arithmetic is not part
    of the type, it's part of operations on values, like in LLVM.
 * (pointer x) is a pointer to a value of type x. Pointers are
    sized regardless of whether x is sized.
 * ADTs where all constructor type-arguments are sized.
Unsized types consist of:
 * (function return-type arg-types...) are functions.
 * (array x) is an array of values of sized type x, of indefinite length
    hopefully tracked by other parts of the program. Unlike C, arrays and
    pointers are distinct types and do not interconvert: see AREF
    for how to use them.
 * ADTs where any constructor type-argument is unsized.

ADTs may (of course) refer to themselves and each other. If this is one way,
there is no problem. However, if there is a recursive dependency, the ADT
is treated as unsized, and furthermore cannot be used directly.

For example, take (defadt foo () (bar (array foo))). This is illegal because
foo is unsized within its own definition. (defadt foo () (bar foo)) is also
illegal, even though having one unsized member would usually be okay.
(defadt foo () (bar (pointer foo))) is okay. (defadt foo (x) (bar (array x))) is okay;
(foo (foo bool)) is not, but that's because an array of arrays isn't valid because
arrays are unsized.

The types byte and word are predefined. They are integer types for
the target architecture's smallest addressable unit and most
conveniently addressable unit, respectively. byte-bits and word-bits
constants are integers with the given n's.

The type bool is predefined as (int 1). Constants true and false are
available; their values as integers are not defined.

Now the actual forms are in the bodies of functions.
A form is either
 * A symbol, referring to either a local variable or a global thing
 * A cons
   ** (let (symbol form) form*) binds a local variable
   ** (if condition then else): Obvious. Condition must be a bool.
   ** (seq form*): progn
   ** (case form ((constructor var*) form*)*) deconstructs an ADT value.
      Like Haskell. If form isn't typed to a sized ADT, type error.
   ** (case! form ((constructor var*) form*)*) is like case, but form must
      evalaute to a pointer to an ADT. The vars are bound to pointers
      to the constructor fields. The ADT may be sized or unsized.
      Passing a pointer in to case! counts as dereferencing it.
   ** (with (var) form*) allocates a value on the stack.
      The variable named symbol is bound to a pointer to a value of
       the inferred type, which must be sized.
      If the pointer is dereferenced after the with form,
       the consequences are undefined.
   ** (with-array (var n) form*) allocates an array of values on
       the stack. n is an integer-typed form, or an integer literal.
      The type of the variable must be inferred to be an unsized type,
       and more specifically, either an array or an unsized ADT.
      In the latter case n is used recursively with that member.
   ** otherwise, a function call. The callee must be a function pointer
       with types matching the arguments.
 * A literal
   ** An integer. This has polymorphic type, e.g. 4 could be of
      any (int n) for n >= 3. Negatives illegal for now.
   ** A constructed literal: (constructor literal*)
      Also polymorphic.

Some functions are built in. In the below, a, b... are universally
quantified type variables, and int is an int type.
 * (pointer a) -> a: (! x) Dereference a pointer.
   Doing this where a is an array is a type error.
 * a, (pointer a) -> Inert: (set! v x) is like "*x = v".
 * (pointer (array a)), int -> (pointer a): (aref a i)
   Get a pointer to the int-th element of the given array. This is the
    only valid primitive on arrays.
   Consequences undefined if you read out of bounds.
 * (pointer a) -> (pointer (array byte)): (bytecast pointer)
   Get a byte representation of any object in memory.
 * (pointer (array byte)) -> (pointer a): (castbyte pointer)
   Get an object from its byte representation. If that representation
    was not originally produced by bytecase and then unmodified,
    consequences are undefined.

FIXME: No way to initialize an unsized adt. It can be a union! case! can't do
shit! Bad bad. Rethink case!, to disambiguate a union it essentially has to
dereference but the whole poitn should be that it doesn't.
But that can't be the point. It doesn't make any sense with having unions.
A person might want to return a union that's unsized and that's okay.
Add a new form to force:
 (pointers form (constructor var ...) form*)
Evaluate form, which must be a (pointer x) where x is the adt constructor is in. vars are bound to the pointers of the slots and forms are evaluated.
Forces the pointer to point to the particular constructor type, as expected.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions
;;;

;;; Tried to compute sizedp of a var.
(define-condition tvar-sizedp (error)
  ((%tvar :initarg :tvar :reader tvar-sizedp-tvar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cache for types.
;;; It's convenient for equivalent types to be EQ.
;;;

(defclass type-cache ()
  (;; Hash from nonnegative integers to types. Could be a vector...
   (%ints :initform (make-hash-table)
          :reader type-cache-ints)
   ;; Hash from types to types for pointing to them.
   (%pointers :initform (make-hash-table :test #'eq
                                         #+sbcl :weakness #+sbcl :key)
              :reader type-cache-pointers)
   ;; Hash from lists (return ...params) to function types.
   (%funs :initform (make-hash-table :test #'equal)
          :reader type-cache-funs)
   ;; Hash from types to types of arrays of them to them.
   (%arrays :initform (make-hash-table :test #'eq
                                       #+sbcl :weakness #+sbcl :key)
            :reader type-cache-arrays)
   ;; Hash from lists (adt-def ...types) to adts
   (%adts :initform (make-hash-table :test #'equal)
          :reader type-cache-adts)))

(defvar *type-cache*)

(defmacro cached-int (n)
  `(gethash ,n (type-cache-ints *type-cache*)))
(defmacro cached-pointer (ty)
  `(gethash ,ty (type-cache-pointers *type-cache*)))
(defmacro cached-fun (key)
  `(gethash ,key (type-cache-funs *type-cache*)))
(defmacro cached-array (ty)
  `(gethash ,ty (type-cache-arrays ty)))
(defmacro cached-adt (key)
  `(gethash ,key (type-cache-adts *type-cache*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Types (again, hare style)
;;;

;;; abstract
(defclass type () ())

(defgeneric sizedp (type))

;; Apply function to type and its component types. Return NIL.
(defgeneric mapnil-type (function type)
  (:argument-precedence-order type function)
  (:method :around (function type) (funcall function type)))
;; Apply function to type. If it returns non-nil, return that.
;; Otherwise, copy type, recursively map-typing any component types.
;; The type doesn't need to be copied if there are no component types.
(defgeneric map-type (function type)
  (:argument-precedence-order type function)
  (:method :around (function type)
    (or (funcall function type) (call-next-method))))

;;; integer type. length is in bits.
;;; (int 1) doubles as bool.
(defclass int (type)
  ((%length :initarg :length :accessor int-type-length
            :type (integer 0))))
(defun make-int (len)
  (or (cached-int len)
      (setf (cached-int len) (make-instance 'int :length len))))
(defun make-bool () (make-int 1))
(defmethod sizedp ((type int)) t)
(defmethod map-type (function (type int))
  (declare (ignore function))
  type)

;;; pointer.
(defclass pointer (type)
  ((%under :initarg :under :accessor pointer-type-underlying :type type)))
(defun make-pointer (ty)
  (or (cached-pointer ty)
      (setf (cached-pointer ty) (make-instance 'pointer :under ty))))
(defmethod sizedp ((type pointer)) t)
(defmethod map-type (function (type pointer))
  (make-pointer (map-type function (pointer-type-underlying type))))
(defmethod mapnil-type (function (type pointer))
  (mapnil-type function (pointer-type-underlying type)))

;;; a function
(defclass fun (type)
  ((%return :initarg :return :accessor fun-return :type type)
   ;; A list of types
   (%params :initarg :params :accessor parameters :type list)))
(defun make-fun (ret params)
  (let ((key (list* ret params)))
    (or (cached-fun key)
        (setf (cached-fun key) (make-instance 'fun
                                 :return ret :params params)))))
(defmethod sizedp ((type fun)) nil)
(defmethod map-type (function (type fun))
  (make-fun (map-type function (fun-return type))
            (loop for param in (parameters type)
                  collect (map-type function param))))
(defmethod mapnil-type (function (type pointer))
  (mapnil-type function (fun-return type))
  (loop for param in (parameters type)
        do (mapnil-type function param)))

(defclass arrayt (type)
  ((%element-type :initarg :et :accessor arrayt-element-type :type type)))
(defun make-arrayt (ty)
  (or (cached-array ty)
      (setf (cached-array ty) (make-instance 'arrayt :et ty))))
(defmethod sizedp ((type arrayt)) nil)
(defmethod map-type (function (type arrayt))
  (make-arrayt (map-type function (arrayt-element-type type))))
(defmethod mapnil-type (function (type arrayt))
  (mapnil-type function (array-element-type type)))

;;; Type placeholder used in a few things.
(defclass tvar (type)
  (;; For debugging
   (%name :initarg :name :accessor name :type symbol :initform nil)))
(defun make-tvar (&optional name) (make-instance 'tvar :name name))
;;; This isn't a real type, so trying to compute its size is a problem.
(defmethod sizedp ((type tvar))
  (error 'tvar-sizedp :tvar type))
(defmethod map-type (function (type tvar))
  (declare (ignore function))
  type)

;;; A "subst" is an alist (tvar . type) used to describe substitutions.
(defun empty-subst () nil)

;;; Given a subst and a type, make a new type in which all the
;;; tvars are replaced by the associated type. (Type doesn't have to be new
;;; if there are no changes.)
(defun subst-type (subst type)
  (map-type (lambda (type)
              (when (typep type 'tvar)
                (let ((pair (assoc type subst :test #'eq)))
                  (if pair (cdr pair) nil))))
            type))

(defun compose-subst (subst1 subst2)
  ;; note: diehl uses Map.union, which i believe takes the left
  ;; item if there's a duplicate. CL union leaves this undefined.
  (union (mapcar (lambda (pair)
                   (cons (car pair) (subst-type subst1 (cdr pair))))
                 subst2)
         subst1
         :key #'car))
(defun compose-substs (&rest substs)
  (cond ((null substs) (empty-subst))
        ((null (rest substs)) (first substs))
        (t (compose-subst (first substs)
                          (apply #'compose-substs (rest substs))))))

;;;;;;;;;;;;;;;;;;;;
;;;
;;; ADT stuff
;;;

;;; Definition of an ADT schema thing, as from defadt. Not a type.
(defclass adt-def ()
  ((%name :initarg :name :accessor name :type symbol)
   ;; A list of bound type variables (tvars)
   (%tvars :initarg :tvars :accessor tvars :type list)
   ;; A list of constructor names (symbols)
   (%constructors :initarg :constructors :accessor constructors :type list)
   ;; A list of (type*), corresponding to the constructors.
   (%members :initarg :members :accessor members :type list)))

;;; E.g., if we have (defadt foo (x) ...), and then refer to (foo (int 3))
;;; somewhere, we have one of these, with args = ((int 3)) but it's a type.
(defclass adt (type)
  (;; Which ADT definition this is an instantiation of.
   (%def :initarg :def :accessor adt-def :type adt-def)
   ;; A list of types.
   (%args :initarg :args :accessor adt-args :type list)))
(defun make-adt (def args)
  (let ((key (list* def args)))
    (or (cached-adt key)
        (setf (cached-adt key) (make-instance 'adt :def def :args args)))))
;;; Due to recursion and stuff, sizedp is a little involved.
(defvar *sizedp-recurrence* nil)
(defmethod sizedp ((type adt))
  (if (member type *sizedp-recurrence* :test #'eq)
      (error 'recursive-sizedp :type type)
      (let* ((def (adt-def type))
             (tvars (tvars def))
             (members (members def))
             (args (adt-args type))
             (map (mapcar #'cons tvars args))
             (*sizedp-recurrence* (cons type *sizedp-recurrence*)))
        ;; What we want to do is check the sizedps of all the elements
        ;; in each constructor. If there are zero, we have sizedp = T.
        ;; If there's at least one in some constructor, sizedp = NIL.
        ;; If there's more than one in one constructor, error.
        (loop with sizedp = T
              for member in members
              for tys = (loop for ty in member
                              collect (subst-type map ty))
              for sizedp-discount = (count-if-not #'sizedp tys)
              do (cond ((zerop sizedp-discount))
                       ((= sizedp-discount 1) (setf sizedp nil))
                       (t (error 'invalid-adt :type type)))
              finally (return sizedp)))))
(defmethod map-type (function (type adt))
  (make-adt (adt-def type)
            (loop for ty in (adt-args type)
                  collect (map-type function ty))))
(defmethod mapnil-type (function (type adt))
  (loop for ty in (adt-args type) do (map-type function ty)))

;;; This is like instantiating a schema, below.
;;; It makes a fresh type variable for each variable bound in the def,
;;; substitutes them into the members, and then returns those.
;;; As a second value it returns the instantiated ast.
;;; That is, it returns a list of lists of types.
(defun instantiate-adt-def (adt-def)
  (let* ((old (tvars adt-def))
         (new (loop for tvar in old collect (make-tvar (name tvar))))
         (map (mapcar #'cons old new)))
    (values
     (flet ((substt (type) (subst-type map type)))
       (loop for member in (members adt-def)
             collect (mapcar #'substt member)))
     (make-adt adt-def new))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type schemata
;;; AKA polytypes. They are NOT types in the above way.
;;; After all, a value can't be a polytype. A polytype must be instantiated.
;;;

(defclass schema ()
  (;; A list of tvars.
   (%tvars :initarg :tvars :accessor tvars :type list)
   ;; The underlying type, in which the tvars are available for use.
   (%type :initarg :type :accessor type :type type)))

;;; Convenience
(defun schema (type &optional tvars)
  (make-instance 'schema :tvars tvars :type type))

;;; Return a list of tvariables free in type or schema or whatever.
(defun free (type)
  (let ((tvars nil))
    (mapnil-type (lambda (ty)
                   (when (typep ty 'tvar)
                     (pushnew ty tvars :test #'eq)))
                 type)
    tvars))
(defun free-in-schema (schema)
  (set-difference (free (type schema)) (tvars schema) :test #'eq))

;;; Subst
(defun subst-schema (subst schema)
  ;; Remove any part of the subst that refers to a bound variable.
  (let* ((tvars (tvars schema))
         (sans (remove-if (lambda (ty) (member ty tvars :test #'eq))
                          subst
                          :key #'car)))
    (schema (subst-type sans (type schema)) tvars)))

;;; HM operation. Given a schema, return a type with fresh free variables
;;; for all the schema's old bound variables.
(defun instantiate (schema)
  (let ((map (loop for tvar in (tvars schema)
                   collect (cons tvar (make-tvar (name tvar))))))
    (subst-type map (type schema))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsing types.

;;; Environments are alists (name . type).
;;; ADT environments are alists (name . adt-def).
(defun parse-type (expr env adt-env)
  (etypecase expr
    (symbol ; must be an alias.
     (let ((pair (assoc expr env)))
       (if pair
           (cdr pair)
           (let ((pair (assoc expr adt-env)))
             (if pair
                 (make-adt (cdr pair) nil)
                 (error "Unknown type ~s" expr))))))
    (cons
     (cl:case (car expr)
       ((int)
        (destructuring-bind (len) (cdr expr)
          (check-type len (integer 0))
          (make-int len)))
       ((pointer)
        (destructuring-bind (under) (cdr expr)
          (make-pointer (parse-type under env adt-env))))
       ((array)
        (destructuring-bind (et) (cdr expr)
          (make-arrayt (parse-type et env adt-env))))
       ((function)
        (destructuring-bind (ret &rest params) (cdr expr)
          (make-fun ret (loop for param in params
                              collect (parse-type param env adt-env)))))
       (otherwise
        (let ((pair (assoc (car expr) adt-env)))
          (if pair
              (make-adt (cdr pair)
                        (loop for type in (cdr expr)
                              collect (parse-type type env adt-env)))
              (error "Unknown type ~s" expr))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ASTs for forms
;;;

;;; abstract
(defclass ast ()
  ((%type :accessor type :initarg :type :type (or type null))))

(defclass seq ()
  (;; A list of ASTs
   (%asts :accessor asts :initarg :asts :type list)))

(defclass call (ast)
  ((%callee :accessor callee :initarg :callee :type ast)
   ;; a list of ASTs
   (%args :accessor args :initarg :args :type list)))

;;; abstract
(defclass literal (ast) ())

(defclass numeric-literal (literal)
  ((%value :accessor numeric-literal-value :initarg :value
           :type (or fixnum float))))

#+(or)(defclass constructor-literal (ast) nil) ; later

(defclass variable (ast)
  ((%name :initarg :name :accessor name :type symbol)))

;; reference to a global symbol (like a C symbol)
(defclass global (variable) ())

;; A local variable or parameter.
(defclass local (variable) ())

;; if
(defclass branch (ast)
  ((%test :initarg :test :accessor test :type ast)
   (%then :initarg :then :accessor then :type ast)
   (%else :initarg :else :accessor else :type ast)))

;; LET with one variable
(defclass bind (ast)
  ((%var :initarg :var :accessor var :type local)
   (%value :initarg :value :accessor value :type ast)
   (%body :initarg :body :accessor body :type ast)))

(defclass with (ast)
  ((%var :initarg :var :accessor var :type local)
   ;; If NIL, this is a WITH. Otherwise, WITH-ARRAY.
   (%len :initarg :len :accessor len :type (or ast null))
   (%body :initarg :body :accessor body :type ast)))

(defclass case (ast)
  ((%value :initarg :value :accessor value :type ast)
   ;; The adts this thing deconstructs.
   (%adt-def :initarg :adt-def :accessor adt-def :type adt-def)
   ;; A list of ((symbol local*) . ast)
   ;; Symbol is the constructor name.
   (%cases :initarg :cases :accessor cases :type list)
   ;; Is this case!, the pointer version?
   (%case!p :initarg :case!p :accessor case!p :type bool)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse forms
;;;

;;; env is an alist (symbol . local). Globals should NOT be included.
;;; adt-env is the same alist (name . adt-def) as in parse-type.
;;; Should we really handle case like this? I'm not sure.
(defun parse-form (form env adt-env)
  (flet ((parse-seq (list env)
           (make-instance 'seq
             :asts (loop for form in list
                         collect (parse-form form env adt-env))))
         (local (name)
           (check-type name symbol)
           (make-instance 'local :name name)))
    (etypecase form
      (symbol
       (or (cdr (assoc form env :test #'eq))
           (make-instance 'global :name form)))
      (cons
       (let ((head (car form)) (args (cdr form)))
         (cl:case head
           ((seq) (parse-seq args env))
           ((let)
            (destructuring-bind ((var value) &rest body) args
              (let ((lvar (local var)))
                (make-instance 'bind
                  :var lvar :value (parse-form value env adt-env)
                  :body (parse-seq body (acons var lvar env))))))
           ((if)
            (destructuring-bind (test then else) args
              (make-instance 'branch
                :test (parse-form test env adt-env)
                :then (parse-form then env adt-env)
                :else (parse-form else env adt-env))))
           ((with)
            (destructuring-bind ((var) &rest body) args
              (let ((lvar (local var)))
                (make-instance 'with
                  :var lvar :len nil
                  :body (parse-seq body (acons var lvar env))))))
           ((with-array)
            (destructuring-bind ((var len) &rest body) args
              (let ((lvar (local var)))
                (make-instance 'with
                  :var (local var) :len (parse-form len env adt-env)
                  :body (parse-seq body (acons var lvar env))))))
           ((case case!)
            (destructuring-bind (value &rest cases) args
              (multiple-value-bind (def cases)
                  (find-adt-def cases adt-env)
                (let ((cases
                        (loop for ((constructor . vars) . body) in cases
                              for lvars = (mapcar #'local vars)
                              for env = (mapcar #'cons vars lvars)
                              collect (cons (cons constructor lvars)
                                            (parse-seq body env)))))
                  (make-instance 'case
                    :value (parse-form value env adt-env)
                    :cases cases
                    :case!p (eq head 'case!)
                    :adt-def def)))))
           (otherwise ; call
            (make-instance 'call
              :callee (parse-form head env adt-env)
              :args (loop for form in args
                          collect (parse-form form env adt-env)))))))
      ;; Back to the typecase - remember that? So long ago
      ((and (integer 0) fixnum)
       (make-instance 'numeric-literal :value form)))))

;;; Given the ((constructor var*) . ast)* list from a case,
;;; return the appropriate adt def, and order the cases to match the def.
(defun find-adt-def (cases env)
  (let* ((constructors (mapcar #'caar cases))
         (def (loop for (ignore . def) in env
                    for oconstructors = (constructors def)
                    ;; when (set-equal constructors oconstructors)
                    when (null (set-exclusive-or constructors oconstructors
                                                 :test #'eq))
                      return def
                    finally (error 'case-unknown :constructors constructors))))
    (values def
            ;; Rearrange the cases.
            (loop for oconstructor in (constructors def)
                  collect (find oconstructor cases
                                :key #'caar :test #'eq)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type inference
;;; A type environment is an alist (variable . schema)
;;; A substitution is an alist (tvar . type)
;;;

;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type environments (tenvs)
;;; Alists (variable . schema)
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

(defmethod infer ((ast local) tenv) (lookup-type ast tenv))
(defmethod infer ((ast global) tenv) (lookup-type ast tenv))
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
           ;; Since there are no local functions, generalization might not
           ;; matter here. But on the other hand if you (let (f global) ...)
           ;; it ought to keep f polymorphic. I don't know.
           (valsc (generalize new-env valt)))
      (multiple-value-bind (bodyt bodysubst)
          (infer (body ast) (extend-tenv (var ast) valsc new-env))
        (values bodyt (compose-subst valsubst bodysubst))))))
(defmethod infer ((ast with) tenv)
  (let ((len (len ast)))
    (multiple-value-bind (lent lensubst)
        (if len
            (infer len tenv)
            (values nil (empty-subst)))
      (declare (ignore lent)) ; FIXME: Assert that it's an int type
      ;; We make and bind a new tvar/scheme for the allocated variable.
      ;; The schema does NOT bind the tvar, because this isn't polymorphic.
      ;; I think this makes sense.
      (let* ((env (subst-tenv lensubst tenv))
             (var (var ast))
             (tvar (make-tvar (name var)))
             (tarr (if len (make-arrayt tvar) tvar))
             ;; This'll force unification with (pointer tvar)
             ;; or (pointer (array tvar)) like we want.
             ;; (I mean, assuming the body uses the thing.)
             (tptr (make-pointer tarr))
             (new-env (extend-tenv (var ast) (schema tptr) env)))
        (multiple-value-bind (bodyt bodysubst)
            (infer (body ast) new-env)
          (values bodyt (compose-subst lensubst bodysubst)))))))
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
                for ((case-cons . locals) . ast) in (cases ast)
                for new-tenv = (extend-tenv-list locals schemata tenv)
                do ;; Make sure everything's in the right order.
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

#|
Case bindings have to be polymorphic I think.
(deftype list (elt) (cons elt (pointer (list elt))) (nil))

(defun map (f x)
  (heapify
   (case x
     ((cons elt next) (cons (f elt) (map f next)))
     ((nil) nil))))

Where (heapify x) = (let (r (malloc)) (set! x r) r)
How is this typed?
Our first type environment is (f:a, x:b, map:forall c. c). Fresh new variables.
A thing without forall is short for having an empty forall.
Then we hit the case. Process each clause. The bound variables are bound to fresh variables in the type environment I think, so for the first clause the type environment will be (elt:d, next:e, f:a, x:b, map:forall c. c). We can infer (f elt) to be g, and update the environment to (elt:d, next:e, f:d->f, x:b, map:forall c. c). (map f next) is inferred to be f and the environment doesn't change since map is polymorphic. The clause overall has type... (cons f g)? Is that a type? I guess. Can't really back-construct the ADT description.
Now, I think, we instantiate the list type. We unify its elt with d and its (pointer (list elt)) with e. So (elt:d, next:(pointer (list d)), f:d->f, x:b, map:forall c. c).
Actually I forgot about x. When we started inferring the clause b should be unified with uh, (cons d e)|nil. So now it's (cons d (pointer (list d))). Now we're cooking with gas.
Maybe we don't generalize. Shit, man.
Following on we'll eventually get a return value of (pointer nil\|(cons d (pointer (list d)))). But that's not a list type. I mean it is, obviously, but the type inference won't know that. I don't think that's acceptable.
Let's go way back to calling cons. Maybe THAT is where we instantiate the ADT.
cons's type is forall elt. (elt, (pointer (list elt))) -> (list elt). Maybe that's it? It's just the fucking constructors that need to be typed well?? Christ. Shit. Fuck.
That's probably not just it. It's probably useful when deconstructing to know the stuff. But still
Fuck dude, what if when deconstructing (cons x y) we just bound them to fresh a and (pointer (list a)) instead of binding y to a fresh variable? fuck dude
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; C translation
;;;

;;; Given a type, return a partial name string for it.
;;; We pretend _ is reserved and use _c to mean special things,
;;; c being some character. Since our types are complicated.
;;; We don't actually need to unparse any of these,
;;; at least for the moment. They just need to be consistent
;;; and unique (i.e. bijective to the actual types).
(defgeneric mangle-type (type))
(defmethod mangle-type ((type int))
  ;; Write int lengths out. In hex. maybe a little contrarian.
  (format nil "i~x" (int-type-length type)))
(defmethod mangle-type ((type pointer))
  (format nil "pointer_~a" (pointer-type-underlying type)))
(defmethod mangle-type ((type fun))
  (concatenate 'string "function"
               (apply #'concatenate 'string
                      "_r" (mangle-type (fun-return type))
                      (loop for p in (parameters type)
                            for i from 0
                            collect (format nil "_a~x~a"
                                            i (mangle-type p))))
               ;; completion marker
               "_d"))
(defmethod mangle-type ((type arrayt))
  (format nil "array_~a" (mangle-type (arrayt-element-type type))))
(defun adt-c-name (adt-name types)
  (concatenate 'string
               (string-downcase (symbol-name adt-name))
               (apply #'concatenate 'string
                      (loop for a in types
                            for i from 0
                            collect (format nil "_a~x~a"
                                            i (mangle-type a))))
               "_d"))
(defmethod mangle-type ((type adt))
  (concatenate 'string
               "struct "
               (adt-c-name (name (adt-def type)) (adt-args type))))

;;; Given an ADT definition and types for its variables, return a C name.
(defun mangle-adt-def (adt-def types)
  (concatenate 'string
               (string-downcase (symbol-name (name adt-def)))
               (apply #'concatenate 'string
                      (loop for a in types
                            for i from 0
                            collect (format nil "_a~x~a"
                                            i (mangle-type a))))
               "_d"))

;;; Given a string variable name, and a type,
;;; return a declaration of the variable.
;;; NOTE: I'd like to just have the c-type-name function,
;;; but thanks to declaration-follows-use this is not sane in general.
(defgeneric c-variable-declaration (name type)
  (:argument-precedence-order type name))

;;; Return a C string representing the given type.
;;; Not useful for some types (e.g. functions)
(defgeneric c-type-name (type))

(defmethod c-type-name ((type int))
  (let ((len (int-type-length type)))
    (cond ((<= len 8) "uint_least8_t")
          ((<= len 16) "uint_least16_t")
          ((<= len 32) "uint_least32_t")
          ((<= len 64) "uint_least64_t")
          (t (error "int type not supported: ~d is too many bits"
                    len)))))

(defun easy-declaration (name type)
  (format nil "~s ~s;" (c-type-name type) name))
(defmethod c-variable-declaration (name (type int))
  (easy-declaration name type))

(defun function-pointer-declaration (name return-type param-types
                                     nptr arrayp)
  (format nil ))

(defmethod c-type-name ((type pointer))
  ;; Does not work for function pointers
  (format nil "~s*" (c-type-name (pointer-type-underlying type))))
(defmethod c-variable-declaration ((type pointer))
  (let ((under 
(defmethod c-variable-declaration ((type fun)) ...)
(defmethod c-variable-declaration (name (type arrayt))
  ;; NOTE: This only makes sense when laying out a struct-
  ;; with-array has to be handled more specially.
  ;; FIXME: Arrays of function pointers are an urban legend
  (format nil "~s ~s[];" (c-type-name (arrayt-element-type type)) name))
(defmethod c-type-name ((type adt))
  (format nil "struct ~s"
          (adt-c-name (name (adt-def type)) (adt-args type))))

(defmethod c-variable-declaration (name (type adt))
  (easy-declaration name type))

;;; Given an ADT's c-name, constructors, and members, return a C type
;;; declaration for it. All type variables must have been substituted
;;; already at this point.
(defun adt-def->c-aux (c-name constructors members)
  (flet ((constructor-struct-name (name types)
           (concatenate 'string
                        "struct "
                        (string-downcase
                         (symbol-name name))
                        " { "
                        (apply #'concatenate
                               'string
                               (loop for i from 0
                                     for type in types
                                     collect (c-type-name type)
                                     collect (format nil " a~x; " i)))
                        "}")))
  (let* ((consstructs ; constructor structs
           (loop for constructor in constructors
                 for types in members
                 collect (constructor-struct-name constructor types)))
         (union
           (concatenate
            'string
            "union { "
            (apply #'concatenate
                   'string
                   (loop for i from 0
                         for consstruct in consstructs
                         collect consstruct
                         collect (format nil " u~x; " i)))
            "}")))
    (concatenate 'string "struct "
                 c-name " { "
                 (if (> (length consstructs) 1) ; we need to disambiguate.
                     "int tag; "
                     "")
                 union " dat; };"))))
(defun adt-def->c (adt-def types)
  (let ((subst (mapcar #'cons (tvars adt-def) types))
        (c-name (adt-c-name (name adt-def) types)))
    (adt-def->c-aux
     c-name
     (constructors adt-def)
     (loop for member in (members adt-def)
           collect (loop for type in member
                         collect (subst-type subst type))))))

;;; Generate C code to compute the expression.
;;; Store the result in destvar (a string).
;;; Returns statements, not an expression, because C.
(defgeneric translate (ast destvar))

(defun symbol->c (symbol)
  (string-downcase (substitute #\_ #\- (symbol-name symbol))))

(defmethod translate ((ast global) destvar)
  (format nil "~s = ~s;" destvar (symbol->c (name ast))))

(defmethod translate ((ast branch) destvar)
  (format nil "{ bool cond; ~s if (cond) { ~s } else { ~s } }"
          (translate (test branch) "cond")
          (translate (then branch) destvar)
          (translate (else branch) destvar)))

(defmethod translate ((ast bind) destvar)
  (multiple-value-bind (declaration c-var-name)
      (let ((var (var ast)))
        (c-variable-declaration (name var) (type var)))
    (format nil "{ ~s ~s ~s }"
            declaration (translate (value ast) c-var-name)
            (translate (body ast) destvar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interpreting toplevel forms
;;; Given a list of toplevel forms, we process, in order:
;;; 1) deftype
;;; 2) defadt
;;; 3) defun, defvar
;;; This isn't part of the language definition, except in that defadt
;;;  never depends on a defun, etc.
;;;

(defclass toplevel () ())

(defclass toplevel-defun (toplevel)
  ((%name :accessor name :initarg :name :type symbol)
   ;; list of locals
   ;; FIXME: Should allow ad hoc polymorphism yada yada
   (%params :accessor params :initarg :params :type list)
   (%body :accessor body :initarg :body :type ast)))

(defun parse-defun (defun adt-env)
  (destructuring-bind (defun name params . body) defun
    (declare (ignore defun))
    (let ((locals (loop for param in params
                        collecting (make-instance 'local :name name))))
      (make-instance 'toplevel-defun
                     :name name :params locals
                     :body (parse-form `(seq ,@body)
                                       (mapcar #'cons params locals)
                                       adt-env)))))

(defun parse-defadt (adt-def form adt-env)
  (destructuring-bind (defadt name tvars &rest members) form
    (declare (ignore defadt name)) ; handled already
    (let ((env (mapcar #'cons tvars (tvars adt-def))))
      (loop for (constructor . types) in members
            collect constructor into constructors
            collect (loop for type in types
                          collect (parse-type type env adt-env))
              into adt-members
            finally (setf (constructors adt-def) constructors
                          (members adt-def) adt-members))))
  (values))

;;; Input is a bunch of (defadt name (tvar*) (constructor type*)*) forms
(defun parse-defadts (forms)
  (loop for form in forms
        for name = (second form)
        for vars = (third form)
        for tvars = (loop for var in vars
                          collect (make-tvar var))
        for def = (make-instance 'adt-def :name name :tvars tvars)
        collect (cons def form) into todo
        collect (cons name def) into adt-env
        finally (loop for (def . form) in todo
                      do (parse-defadt def form adt-env))
                (return adt-env)))

#+(or)
(defun process-toplevels (toplevels)
  ;; dut dut not actually doing anything but defuns yet
  (let (deftypes defadts defuns)
    (loop for toplevel in toplevels
          do (ecase (car toplevel)
               #+(or)((deftype) (push toplevel deftypes))
               #+(or)((defadt) (push toplevel defadts))
               ((defun)
                (push (parse-defun toplevel nil) defuns))))
    (let* (#|process defadts and deftypes here...|#
           ;; Our initial type environment is simple:
           ;; every name is bound to a polymorphic pointer.
           ;; Hypothetically we could bind functions specifically to functions,
           ;; but strictly speaking it shouldn't be necessary...
           (fun-tvars (loop for defun in defuns
                            for name = (name defun)
                            collect (make-tvar name)))
           (fun-types (mapcar #'make-pointer fun-tvars))
           (tenv (loop for defun in defuns
                       for fun-tvar in fun-tvars
                       for fun-type in fun-types
                       for name = (name defun)
                       collect (cons name (schema fun-type (list fun-tvar))))))
      ;; Loop through asts and inference them.
      ;; When we get a type for a function's body,
      ;; unify it with (function ret-type paramtypes...)
      ;; and apply that substitution to the whole env.
      (loop for defun in defuns
            for fun-type in fun-types
            for params = (params defun) ; list of locals, remember
            for tvars = (loop for param in params
                              collecting (make-tvar (name param)))
            for schemata = (mapcar #'schema tvars)
            for local-tenv = (extend-tenv-list params schemata tenv)
            ;; Now infer
            do (multiple-value-bind (ret-type subst)
                   (infer (body defun) local-tenv)
                 (let* ((new-paramtypes
                          (loop for tvar in tvars
                                collect (subst-type subst tvar)))
                        (ftype (make-fun ret-type new-paramtypes))
                        (funsubst (unify fun-type ftype))
                        (subst (compose-subst fun-subst subst)))
                   ;; Update the tenv!
                   (setf tenv (subst-tenv subst tenv)))))
      ;; Now we have defuns with typed bodies.
      ;; All we have to do now is dump them out. Ha. Ha.
      )))
