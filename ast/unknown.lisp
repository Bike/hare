(in-package #:hare.ast)

;;; This is a pseudo-AST marking a parse failure, e.g. due to whether something
;;; is a macro being unknown. It will be change-class'd into a real AST.
(defclass unknown (ast)
  ((%expr :initarg :expr :accessor expr)
   ;; the env is kept around to make reparsing later more convenient.
   (%env :initarg :env :accessor environment)))

;;; Mutate an UNKNOWN into being a real AST.
(defgeneric transform-unknown (unknown ast)
  (:argument-precedence-order ast unknown))

;;; Note that none of these transfer the type. When this function is used the
;;; UNKNOWN will probably not have one. But even if it did, change-class would
;;; just keep it where it is, since it's a slot the source and destination
;;; classes both have.
(defmethod transform-unknown ((unknown unknown) (ast seq))
  (change-class unknown 'seq :asts (asts ast) :value (value ast)))
(defmethod transform-unknown ((unknown unknown) (ast call))
  (change-class unknown 'call :callee (callee ast) :args (args ast)))
(defmethod transform-unknown ((unknown unknown) (ast literal))
  (change-class unknown 'literal :initializer (initializer ast)))
(defmethod transform-unknown ((unknown unknown) (ast reference))
  (change-class unknown 'reference :variable (variable ast)))
(defmethod transform-unknown ((uk unknown) (ast bind))
  (change-class uk 'bind :bindings (bindings ast) :body (body ast)))
(defmethod transform-unknown ((uk unknown) (ast with))
  (change-class uk 'with :variable (variable ast)
                         :nelements (nelements ast) :body (body ast)))
(defmethod transform-unknown ((uk unknown) (ast initialize))
  (change-class uk 'initialize :value (value ast)
                               :initializer (initializer ast)))
(defmethod transform-unknown ((uk unknown) (ast primitive))
  (change-class uk 'primitive :name (name ast) :args (args ast)))
(defmethod transform-unknown ((uk unknown) (ast case))
  (change-class uk 'case :value (value ast) :adt-def (adt-def ast)
                         :clauses (clauses ast) :case!p (case!p ast)))
(defmethod transform-unknown ((uk unknown) (ast construct))
  (change-class uk 'construct :constructor (constructor ast)
                :args (args ast)))
