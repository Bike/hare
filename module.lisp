(in-package #:hare)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modules
;;; As described in the README, modules contain compile-time and polytyped
;;; information.

(defclass module ()
  ;; FIXME: Should store constants.
  ;; Also type aliases once those are implemented.
  ((adt-env :initarg :adt-env :accessor adt-env :type adt-env)
   ;; A list of (variable type string), the last being a C identifier.
   ;; (Or maybe later a shared object identifier.)
   (exports :initarg :exports :accessor exports :type list)
   ;; A list of (variable type string) like exports.
   (externs :initarg :externs :accessor externs :type list)
   ;; BINDINGS is an alist from variables to initializers.
   ;; FIXME: Could be made into an actual structure.
   (bindings :initarg :bindings :accessor bindings :type list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsing functions
;;;

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

;;; Input is a list of (defadt name (tvar*) (constructor type*)*) forms.
;;; Return an adt environment.
(defun parse-defadts (forms)
  (loop with adt-env = (make-adt-env)
        for form in forms
        for name = (second form)
        for vars = (third form)
        for tvars = (loop for var in vars
                          collect (make-tvar var))
        for def = (make-instance 'adt-def :name name :tvars tvars)
        collect (cons def form) into todo
        do (add-adt-def def adt-env)
        finally (loop for (def . form) in todo
                      do (parse-defadt def form adt-env)
                         (finish-adt-def def adt-env))
                (return adt-env)))

;;; Given a list of (defconstant name initializer) forms, an env, and an
;;; adt-env, return a new environment extending the given environment
;;; but with the named constants bound to initializers.
(defun parse-defconstants (defconstants env adt-env)
  ;; constants may refer to each other, but not circularly. So if we hit a
  ;; constant we haven't yet seen while parsing an initializer, we go off
  ;; and parse that constant, then try again.
  (let ((cenv env))
    (labels ((parse-defconstant (form)
               (destructuring-bind (def name init) form
                 (declare (ignore def))
                 ;; head off recursion
                 (setf defconstants (delete form defconstants :test #'eq))
                 (tagbody
                  try-again
                    (let ((init
                            (handler-bind
                                ((variable-unbound
                                   (lambda (c)
                                     (let* ((name (name c))
                                            (form (assoc name defconstants)))
                                       ;; no form means the name is undefined,
                                       ;; so just let the error percolate up.
                                       (when form
                                         (parse-defconstant form)
                                         (go try-again))))))
                              (parse-initializer init cenv adt-env))))
                      ;; FIXME: Wasteful to not just mutate the environment
                      (setf cenv (make-env (list name) (list init) cenv)))))))
      (loop until (null defconstants)
            do (parse-defconstant (first defconstants)))
      cenv)))

(defun make-bindings (defvars env adt-env)
  (loop for (_ name . rest) in defvars
        for var = (lookup name env)
        for init = (if (null rest)
                       (undef)
                       (parse-initializer (first rest) env adt-env))
        collect (cons var init)))

(defun initial-env (defvars-and-externs)
  (loop for (_ name) in defvars-and-externs
        collect name into names
        collect (make-instance 'variable :name name) into vars
        finally (return (make-env names vars))))

(defun parse-export (decl var-env type-env adt-env)
  (destructuring-bind (export name type c-name)
      decl
    (declare (ignore export))
    (check-type name symbol)
    (check-type c-name string)
    (list (lookup name var-env) (parse-type type type-env adt-env) c-name)))

(defun parse-exports (decls var-env type-env adt-env)
  (loop for decl in decls
        collect (parse-export decl var-env type-env adt-env)))

(defun parse-extern (decl var-env type-env adt-env)
  (destructuring-bind (extern name type c-name)
      decl
    (declare (ignore extern))
    (check-type name symbol)
    (check-type c-name string)
    (list (lookup name var-env)
          (parse-type type type-env adt-env)
          c-name)))

(defun parse-externs (decls var-env type-env adt-env)
  (loop for decl in decls
        collect (parse-extern decl var-env type-env adt-env)))

(defun divide-toplevel-forms (forms)
  (let (defadts defvars defconstants exports externs)
    (loop for form in forms
          do (cl:case (car form)
               ((defadt) (push form defadts))
               ((defvar) (push form defvars))
               ((defconstant) (push form defconstants))
               ((export) (push form exports))
               ((extern) (push form externs))
               (otherwise
                (error "Unknown toplevel form: ~a" form))))
    (values defadts defvars defconstants exports externs)))

;;; FORMS is a list of top level forms.
;;; Output is a module object.
(defun parse-module (forms)
  (multiple-value-bind (defadts defvars defconstants exports externs)
      (divide-toplevel-forms forms)
    (let* ((adt-env (parse-defadts defadts))
           (env (initial-env (append defvars externs)))
           (pexterns (parse-externs externs env nil adt-env))
           (cenv (parse-defconstants defconstants env adt-env))
           (exports (parse-exports exports env nil adt-env))
           (bindings (make-bindings defvars cenv adt-env)))
      (make-instance 'module
        :adt-env adt-env
        :exports exports
        :externs pexterns
        :bindings bindings))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Manifestation
;;;
;;; Given a module and a list of (var . concrete-type) bindings that are
;;; desired, returns a list of (var (concrete-type . initializer)*) bindings.
;;; I'm leaving the possibility of multiple distinct initializers for the sake
;;; of ad-hoc polymorphism, though at the moment that's not allowed.
;;; A type is "concrete" if it has no free type variables.
;;;

;;; so basically we shouldn't do any inference except relative to these concrete
;;; types cos why else bother tbh.............
;;; Though we will probably be redoing work. Might have to think about it.
(defun manifest (module desired)
  (let* ((result nil)
         (bindings (bindings module))
         ;; Type environment for the defvars.
         ;; Max polymorphic recursion, so every name can be any pointer.
         ;; (Of course, these may fail to unify with any definitions present.)
         (tenv1 (loop for (var . _) in bindings
                      for tvar = (make-tvar (name var))
                      for tptr = (make-pointer tvar)
                      for schema = (schema tptr (list tvar))
                      collect (cons var schema)))
         ;; Type environment from extern declarations
         (externs (externs module))
         (extern-vars (mapcar #'first externs))
         (tenv2 (loop for (var type _) in externs
                      for tptr = (make-pointer type)
                      for schema = (schema tptr)
                      collect (cons var schema)))
         (tenv (append tenv1 tenv2)))
    ;; worklist structure: just keep adding new instances to instantiate
    (loop until (null desired)
          do (destructuring-bind (var . type) (pop desired)
               ;; If we've done this already, ignore.
               (let ((existing (assoc var result)))
                 (unless (and existing
                              (assoc type (cdr existing)))
                   ;; OK go.
                   (multiple-value-bind (init new)
                       (new-instances var type bindings tenv extern-vars)
                     (setf desired (append desired new))
                     ;; Put the new initializer into the result.
                     (if existing
                         (push (cons type init) (cdr existing))
                         (push (list var (cons type init)) result)))))))
    result))

;; Take a variable, a concrete type it needs, the list of variable
;; bindings, and the type environment.
;; Return two values: an initializer,
;; and a list of (var . type) that need instantiation.
(defun new-instances (var type bindings tenv bound)
  (let* ((binding (assoc var bindings))
         (init (or (cdr binding) (error "Who?"))))
    ;; we use just the basic tenv b/c polymorphic recursion.
    ;; even the variable we're inferring for could have a different type.
    ;; NOTE TO SELF: infer-instances needs to walk through the initializer,
    ;; and return an alist of variables and their types.
    ;; And they need to be concrete types, since the input type is.
    (values init (infer-instances init type tenv bound))))

;; Return a list of variable-initializers and references that are free.
(defun initializer-free-varthings (initializer bound)
  (let ((refs nil))
    (mapnil-initializer
     (lambda (initializer)
       (typecase initializer
         (variable-initializer
          (unless (member (variable initializer) bound)
            (push initializer refs)))
         (lambda-initializer
          (setf refs
                (nconc refs
                       (free-references (body initializer)
                                        (append bound
                                                (params initializer))))))))
     initializer)
    refs))

(defun infer-instances (initializer concrete tenv bound)
  ;; FIXME: Add in assertions that the concrete type is actually concrete,
  ;; and that the later types are too.
  ;; KLUDGE: We can infer all the initializers eagerly, earlier.
  (unless (slot-boundp initializer '%type)
    (infer-initializer-toplevel initializer tenv))
  (let* ((free (initializer-free-varthings initializer bound))
         (abstract (type initializer))
         ;; NOTE: If UNIFY is changed later to work by side effects
         ;; for efficiency, or anything like that,
         ;; this call still has to be non-side-effectful since it
         ;; will be done more than once if an initializer needs to
         ;; be instantiated with multiple concrete types.
         (subst (unify abstract concrete)))
    (loop for thing in free ; variable initializers and reference ASTs
          for variable = (variable thing)
          ;; Concretize the type
          ;; The reference will be of a pointer, so undo that.
          for type = (subst-type subst (pointer-type-underlying (type thing)))
          ;; FIXME: Assertion that type is concrete goes around here.
          for existing = (assoc variable result)
          if (null existing)
            collect (list variable type) into result
          else
            unless (member type (rest existing) :test #'type=)
              do (push type (cdr existing))
          finally ; convert into simple (variable . type) pairs
                  (return
                    (loop for (variable . types) in result
                          nconcing (loop for type in types
                                         collecting (cons variable type)))))))
