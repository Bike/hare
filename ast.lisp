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
dereference but the whole point should be that it doesn't.
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
  ((%type :accessor type :initarg :type :type type)))

;;; Apply function to AST and its component ASTs. Return value undefined.
(defgeneric mapnil-ast (function ast)
  (:argument-precedence-order ast function)
  (:method :before (function (ast ast)) (funcall function ast)))
;;; Apply function to ast. If it returns non-nil, return that.
;;; Otherwise, copy the ast, recursively map-ast-ing any component ASTs.
;;; MUST copy, even if there are no components.
(defgeneric map-ast (function ast)
  (:argument-precedence-order ast function)
  (:method :around (function (ast ast))
    (or (funcall function ast) (call-next-method))))

(defun mapnil-asts (function asts)
  (loop for ast in asts do (mapnil-ast function ast)))
(defun map-asts (function asts)
  (loop for ast in asts collect (map-ast function ast)))

(defclass seq ()
  (;; A list of ASTs
   (%asts :accessor asts :initarg :asts :type list)))
(defmethod mapnil-ast (function (ast seq))
  (mapnil-asts function (asts ast)))
(defmethod map-ast (function (ast seq))
  (make-instance 'seq :asts (map-asts function (asts seq))))

(defclass call (ast)
  ((%callee :accessor callee :initarg :callee :type ast)
   ;; a list of ASTs
   (%args :accessor args :initarg :args :type list)))
(defmethod mapnil-ast (function (ast call))
  (mapnil-ast function (callee ast))
  (mapnil-asts function (args ast)))
(defmethod map-ast (function (ast call))
  (make-instance 'call
    :callee (map-ast function (callee ast))
    :args (map-asts function (args ast))))

;;; This is separate from the initializer, because initializers are
;;; polymorphic. That is, they cannot have a monotype, but any particular
;;; use of them in code can.
(defclass literal (ast)
  (;; This is a "literal" in literals.lisp terms, i.e. a member of
   ;; a restricted subset of initializers.
   (%initializer :initarg :initializer :accessor initializer
                 :type initializer)))
(defmethod mapnil-ast (function (ast literal)) (declare (ignore function)))
(defmethod map-ast (function (ast literal))
  (make-instance 'literal :initializer (initializer ast)))

;;; Separate from variables for the same reason.
(defclass reference (ast)
  ((%variable :initarg :variable :accessor variable :type variable)))
(defmethod mapnil-ast (function (ast reference)) (declare (ignore function)))
(defmethod map-ast (function (ast reference))
  (make-instance 'reference :variable (variable ast)))

;; if
(defclass branch (ast)
  ((%test :initarg :test :accessor test :type ast)
   (%then :initarg :then :accessor then :type ast)
   (%else :initarg :else :accessor else :type ast)))
(defmethod mapnil-ast (function (ast branch))
  (mapnil-ast function (test ast))
  (mapnil-ast function (then ast))
  (mapnil-ast function (else ast)))
(defmethod map-ast (function (ast branch))
  (make-instance 'branch
    :test (map-ast function (test ast))
    :then (map-ast function (then ast))
    :else (map-ast function (else ast))))

;; LET with one variable
(defclass bind (ast)
  ((%variable :initarg :variable :accessor variable :type variable)
   (%value :initarg :value :accessor value :type ast)
   (%body :initarg :body :accessor body :type ast)))
(defmethod mapnil-ast (function (ast bind))
  (mapnil-ast function (value ast))
  (mapnil-ast function (body ast)))
(defmethod map-ast (function (ast bind))
  (make-instance 'bind
    :variable (variable ast)
    :value (map-ast function (value ast))
    :body (map-ast function (body ast))))

;;; This AST is currently used only in a WITH.
;;; It's needed because we need a stable type for the initializer
;;; even though it may have polymorphic components.
(defclass initialization (ast)
  ((%initializer :initarg :initializer :accessor initializer
                 :type initializer)))
(defmethod mapnil-ast (function (ast initialization))
  (declare (ignore function)))
(defmethod map-ast (function (ast initialization))
  (make-instance 'initialization :initializer (initializer ast)))

(defclass with (ast)
  ((%variable :initarg :variable :accessor variable :type variable)
   (%initialization :initarg :initialization :accessor initialization
                    :type initialization)
   (%body :initarg :body :accessor body :type ast)))
(defmethod mapnil-ast (function (ast with))
  (mapnil-ast function (body ast))
  (mapnil-ast function (initialization ast)))
(defmethod map-ast (function (ast with))
  (make-instance 'with
    :variable (variable ast)
    :initialization (map-ast function (initialization ast))
    :body (map-ast function (body ast))))

(defclass case (ast)
  ((%value :initarg :value :accessor value :type ast)
   ;; The adts this thing deconstructs.
   (%adt-def :initarg :adt-def :accessor adt-def :type adt-def)
   ;; A list of ((symbol variable*) . ast),
   ;; symbol being the constructor name.
   (%cases :initarg :cases :accessor cases :type list)
   ;; Is this case!, the pointer version?
   (%case!p :initarg :case!p :accessor case!p :type bool)))
(defmethod mapnil-ast (function (ast case))
  (mapnil-ast function (value ast))
  (loop for (_ . sub) in (cases ast)
        do (mapnil-ast function sub)))
(defmethod map-ast (function (ast case))
  (make-instance 'case
    :value (map-ast function (value ast))
    :adt-def (adt-def ast)
    :case!p (case!p ast)
    :cases (loop for (x . sub) in (cases ast)
                 collect (cons x (map-ast function sub)))))
