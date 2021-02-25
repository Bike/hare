(in-package #:hare)

;; A type substitution, i.e. a mapping from tvars to types.
(defclass subst ()
  (;; An alist (tvar . type)
   (%bindings :initarg :bindings :accessor bindings)))

(defun empty-subst () (make-instance 'subst :bindings ()))

;;; Given a subst and a type, make a new type in which all the
;;; tvars are replaced by the associated type. (Type doesn't have to be new
;;; if there are no changes.)
(defun subst-type (subst type)
  (let ((bindings (bindings subst)))
    (map-type (lambda (type)
                (when (typep type 'tvar)
                  (let ((pair (assoc type bindings :test #'eq)))
                    (if pair (cdr pair) nil))))
              type)))

(defun subst-schema (subst schema)
  ;; Remove any part of the subst that refers to a bound variable.
  ;; We do this without consing up a restricted subst.
  (let ((tvars (tvars schema)) (bindings (bindings subst)))
    (schema (map-type (lambda (type)
                        (when (typep type 'tvar)
                          (let ((pair (assoc type bindings :test #'eq)))
                            (if (and pair (not (member type tvars :test #'eq)))
                                (cdr pair)
                                nil))))
                      (type schema))
            tvars)))

(defun compose-bindings (bindings1 bindings2)
  ;; Apply substitutions from the first to the second, then take the union.
  ;; note: diehl uses Map.union, which i believe takes the left
  ;; item if there's a duplicate. CL union leaves this undefined.
  (union (mapcar (lambda (pair)
                   (cons (car pair)
                         (subst-type bindings1 (cdr pair))))
                 bindings2)
         bindings1
         :key #'car))

(defun compose-subst (subst1 subst2)
  (make-instance 'subst
    :bindings (compose-bindings (bindings subst1) (bindings subst2))))

(defun compose-substs (&rest substs)
  (cond ((null substs) (empty-subst))
        ((null (rest substs)) (first substs))
        (t (make-instance 'subst
             :bindings (reduce #'compose-bindings substs
                               :key #'bindings)))))
