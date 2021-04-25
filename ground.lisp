(in-package #:hare)

;;; This file defines the "ground" environment, which pretty much just contains
;;; the core special operators. NOTE: Things like pointers and arrays can
;;; probably be in separate systems, although realistically very few programs
;;; won't involve them.

(defun make-ground-env ()
  (make-env '(let seq case case! with initialize cons primitive)
            (mapcar #'make-instance
                    (make-list 8 :initial-element 'special-operator-info))))

(defparameter *ground* (make-ground-env))

;;; Shield the ground environment from modification.
(defun make-stdenv () (make-global-env nil nil *ground*))
