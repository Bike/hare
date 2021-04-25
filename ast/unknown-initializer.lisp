(in-package #:hare.ast)

;;; A placeholder that can be parsed again later.
(defclass unknown-initializer (initializer)
  ((%expr :accessor expr :initarg :expr)
   (%env :accessor environment :initarg :env)))

(defgeneric transform-unknown-initializer (unknown initializer)
  (:argument-precedence-order initializer unknown))

(defmethod transform-unknown-initializer ((uk unknown-initializer)
                                          (init integer-initializer))
  (change-class uk 'integer-initializer :value (value init)))
(defmethod transform-unknown-initializer ((uk unknown-initializer)
                                          (init variable-initializer))
  (change-class uk 'variable-initializer :variable (variable init)))
(defmethod transform-unknown-initializer ((uk unknown-initializer)
                                          (init constructor-initializer))
  (change-class uk 'constructor-initializer
                :constructor (constructor init) :fields (fields init)))
(defmethod transform-unknown-initializer ((uk unknown-initializer)
                                          (init undef-initializer))
  (change-class uk 'undef-initializer))
(defmethod transform-unknown-initializer ((uk unknown-initializer)
                                          (init lambda-initializer))
  (change-class uk 'lambda-initializer
                :params (params init) :body (body init)))
(defmethod transform-unknown-initializer ((uk unknown-initializer)
                                          (init array-initializer))
  (change-class uk 'array-initializer :elements (elements init)))
(defmethod transform-unknown-initializer ((uk unknown-initializer)
                                          (init vla-initializer))
  (change-class uk 'vla-initializer :nelements (nelements init)))
