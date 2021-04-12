(in-package #:hare.ast)

#|

A form is either
 * A symbol, referring to either a local variable or a global thing
 * A cons
   * (let (symbol form) form*) binds a local variable
   * (seq form*): progn
   * (case form ((constructor var*) form*)*) deconstructs an ADT value.
     Like Haskell. If form isn't typed to a sized ADT, type error.
   * (case! form ((constructor var*) form*)*) is like case, but form must
     evalaute to a pointer to an ADT. The vars are bound to pointers
     to the constructor fields. The ADT may be sized or unsized.
   * (cons constructor form*) constructs an ADT value.
     This is a special operator that probably won't be exposed in the final
     language definition; instead a higher level defadt will define functions
     to do it. But the compiler has to have some idea of what it's doing.
   * (with (var [initializer]) form*) allocates a value on the stack.
     The variable named symbol is bound to a pointer to a value of
      the inferred type. If this type is unsized, an initializer must
      be provided so the compiler can determine a size and possibly mark
      constructors within.
     If there is no initializer and the pointer is dereferenced before
      a value is stored in it, the consequences are undefined.
     If the pointer is dereferenced after the with form exits,
      the consequences are undefined.
   * otherwise, a function call. The callee must be a function pointer
      with types matching the arguments.
 * A literal (see literals.lisp)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ASTs for forms
;;;

;;; abstract
(defclass ast ()
  ((%type :accessor type :initarg :type :type type:type)))

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

(defclass seq (ast)
  (;; A list of ASTs
   (%asts :accessor asts :initarg :asts :type list)))
(defmethod mapnil-ast (function (ast seq))
  (mapnil-asts function (asts ast)))
(defmethod map-ast (function (ast seq))
  (make-instance 'seq :asts (map-asts function (asts ast))))

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

(defclass variable ()
  ((%name :accessor name :initarg :name :type symbol)))
(defun make-variable (name)
  (check-type name symbol)
  (make-instance 'variable :name name))

(defmethod print-object ((o variable) s)
  (print-unreadable-object (o s :type t)
    (write (name o) :stream s)))

;;; Separate from variables for the same reason.
(defclass reference (ast)
  ((%variable :initarg :variable :accessor variable :type variable)))
(defmethod mapnil-ast (function (ast reference)) (declare (ignore function)))
(defmethod map-ast (function (ast reference))
  (make-instance 'reference :variable (variable ast)))

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

(defclass case-clause ()
  ((%constructor :initarg :constructor :accessor constructor
                 :type type:constructor)
   ;; A proper list of VARIABLEs.
   (%variables :initarg :variables :accessor variables :type list)
   (%body :initarg :body :accessor body :type ast)))

(defclass case (ast)
  ((%value :initarg :value :accessor value :type ast)
   ;; The ADT this thing deconstructs.
   (%adt-def :initarg :adt-def :accessor adt-def :type adt-def)
   ;; A proper list of CASE-CLAUSEs.
   (%clauses :initarg :clauses :accessor clauses :type list)
   ;; Is this case!, the pointer version?
   (%case!p :initarg :case!p :accessor case!p :type boolean)))
(defmethod mapnil-ast (function (ast case))
  (mapnil-ast function (value ast))
  (loop for clause in (clauses ast)
        do (mapnil-ast function (body clause))))
(defmethod map-ast (function (ast case))
  (make-instance 'case
    :value (map-ast function (value ast))
    :adt-def (adt-def ast)
    :case!p (case!p ast)
    :clauses (loop for clause in (clauses ast)
                   collect (make-instance 'case-clause
                             :constructor (constructor clause)
                             :variables (variables clause)
                             :body (map-ast function (body clause))))))

(defclass construct (ast)
  ((%constructor :initarg :constructor :accessor constructor
                 :type type:constructor)
   ;; a list of ASTs
   (%args :initarg :args :accessor args :type list)))
(defmethod mapnil-ast (function (ast construct))
  (mapnil-asts function (args ast)))
(defmethod map-ast (function (ast construct))
  :constructor (constructor ast)
  :args (map-asts function (args ast)))
