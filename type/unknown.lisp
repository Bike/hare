(in-package #:hare.type)

;;; Pseudo-type used when parsing failed.
(defclass unknown (type)
  ((%expr :initarg :expr :accessor expr)
   (%type-env :initarg :type-env :accessor type-env)))

(defgeneric transform-unknown (unknown to)
  (:argument-precedence-order to unknown))

(defmethod transform-unknown ((unknown unknown) (to int))
  (change-class unknown 'int :length (int-type-length to)))
(defmethod transform-unknown ((unknown unknown) (to pointer))
  (change-class unknown 'pointer :under (pointer-type-underlying to)))
(defmethod transform-unknown ((unknown unknown) (to fun))
  (change-class unknown 'fun
                :return (fun-return to) :parameters (parameters to)))
(defmethod transform-unknown ((unknown unknown) (to arrayt))
  (change-class unknown 'fun :et (arrayt-element-type to)))
;; should never need to transform into a tvar
(defmethod transform-unknown ((unknown unknown) (to adt))
  (change-class unknown 'adt :def (adt-def to) :args (adt-args to)))
