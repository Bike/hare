(in-package #:hare)

;; A type substitution, i.e. a mapping from tvars to types.
(defclass tysubst ()
  (;; An alist (tvar . type)
   (%bindings :initarg :bindings :accessor bindings)))

(defmethod print-object ((o tysubst) s)
  (print-unreadable-object (o s :type t)
    (write-char #\( s)
    (loop for ((tvar . type) . rest) on (bindings o)
          do (write-char #\( s)
             (write (name tvar) :stream s)
             (write-char #\Space s)
             (write (unparse-type type) :stream s)
             (write-char #\) s)
          unless (null rest) do (write-char #\Space))
    (write-char #\) s)))

(defun empty-tysubst () (make-instance 'tysubst :bindings ()))

(defun make-tysubst (map) (make-instance 'tysubst :bindings map))

;;; Given a subst and a type, make a new type in which all the
;;; tvars are replaced by the associated type. (Type doesn't have to be new
;;; if there are no changes.)
(defun subst-type (tysubst type)
  (let ((bindings (bindings tysubst)))
    (map-type (lambda (type)
                (when (typep type 'tvar)
                  (let ((pair (assoc type bindings :test #'eq)))
                    (if pair (cdr pair) nil))))
              type)))

(defun subst-schema (tysubst schema)
  ;; Remove any part of the subst that refers to a bound variable.
  ;; We do this without consing up a restricted subst.
  (let ((tvars (tvars schema)) (bindings (bindings tysubst)))
    (schema (map-type (lambda (type)
                        (when (typep type 'tvar)
                          (let ((pair (assoc type bindings :test #'eq)))
                            (if (and pair (not (member type tvars :test #'eq)))
                                (cdr pair)
                                nil))))
                      (type schema))
            tvars)))

(defun compose-tysubst (tysubst1 tysubst2)
  (make-instance 'tysubst
    :bindings
    ;; Apply substitutions from the first to the second, then take the union.
    ;; note: diehl uses Map.union, which i believe takes the left
    ;; item if there's a duplicate. CL union leaves this undefined.
    (union (mapcar (lambda (pair)
                     (cons (car pair)
                           (subst-type tysubst1 (cdr pair))))
                   (bindings tysubst2))
           (bindings tysubst1)
           :key #'car)))

(defun compose-tysubsts (&rest tysubsts)
  (cond ((null tysubsts) (empty-tysubst))
        ((null (rest tysubsts)) (first tysubsts))
        (t (make-instance 'tysubst
             :bindings (reduce #'compose-tysubst tysubsts)))))
