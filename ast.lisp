(in-package #:hare)

#|

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
   ** (with (var [initializer]) form*) allocates a value on the stack.
      The variable named symbol is bound to a pointer to a value of
       the inferred type. If this type is unsized, an initializer must
       be provided so the compiler can determine a size.
      If there is no initializer and the pointer is dereferenced before
       a value is stored in it, the consequences are undefined.
      If the pointer is dereferenced after the with form exits,
       the consequences are undefined.
   ** (with-array (var n) form*) allocates an array of values on
       the stack. n is an integer-typed form, or an integer literal.
      The type of the variable must be inferred to be an unsized type,
       and more specifically, either an array or an unsized ADT.
      In the latter case n is used recursively with that member.
      NOTE: Not implemented yet.
   ** otherwise, a function call. The callee must be a function pointer
       with types matching the arguments.
 * A literal (see literals.lisp)

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

;;; This is separate from the initializer, because initializers are
;;; polymorphic. That is, they cannot have a monotype, but any particular
;;; use of them in code can.
(defclass literal (ast)
  (;; This is a "literal" in literals.lisp terms, i.e. a member of
   ;; a restricted subset of initializers.
   (%initializer :initarg :initializer :accessor initializer
                 :type initializer)))

;;; Separate from variables for the same reason.
(defclass reference (ast)
  ((%variable :initarg :variable :accessor variable :type variable)))

;; if
(defclass branch (ast)
  ((%test :initarg :test :accessor test :type ast)
   (%then :initarg :then :accessor then :type ast)
   (%else :initarg :else :accessor else :type ast)))

;; LET with one variable
(defclass bind (ast)
  ((%variable :initarg :variable :accessor variable :type variable)
   (%value :initarg :value :accessor value :type ast)
   (%body :initarg :body :accessor body :type ast)))

(defclass with (ast)
  ((%variable :initarg :variable :accessor variable :type variable)
   (%initializer :initarg :initializer :accessor initializer
                 :type initializer)
   (%body :initarg :body :accessor body :type ast)))

(defclass case (ast)
  ((%value :initarg :value :accessor value :type ast)
   ;; The adts this thing deconstructs.
   (%adt-def :initarg :adt-def :accessor adt-def :type adt-def)
   ;; A list of ((symbol variable*) . ast),
   ;; symbol being the constructor name.
   (%cases :initarg :cases :accessor cases :type list)
   ;; Is this case!, the pointer version?
   (%case!p :initarg :case!p :accessor case!p :type bool)))
