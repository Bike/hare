(in-package #:hare)

;;; Stuff for turning sexprs into modules

(defun process-particulars (particulars env tyenv)
  (loop for (name tyexpr c-name) in particulars
        for var = (variable (lookup name env))
        for ty = (parse-type tyexpr tyenv)
        collect (if c-name (list var ty c-name) (list var ty))))

(defun sexprs-manifest (sexprs particulars)
  (let* ((prem (parse-pre-module sexprs))
         (env (environment prem))
         (tyenv (type-env prem))
         (mod (module prem))
         (particulars (process-particulars particulars env tyenv)))
    (manifest mod particulars)))

;;; Stuff for turning modules into target code

(defclass backend () ())

(defgeneric dump (manifest backend &key to-file)
  (:argument-precedence-order backend manifest)
  ;; convenience
  (:method ((manifest manifest) (backend symbol) &rest keys)
    (apply #'dump manifest (make-instance backend) keys)))

;;; Combined

(defun build (sexprs particulars backend &rest kwargs)
  (type:with-type-cache ()
    (apply #'dump
           (sexprs-manifest sexprs particulars)
           backend
           kwargs)))

#|
(build '((defvar main (lambda (argc argv) (puts (primitive ref hello 0))))
         (declaim (type () (function (int 32) (pointer (int 8))) puts) (variable puts))
         (defvar hello (array 72 101 108 108 111 44 32 119 111 114 108 100 33 0)))
       '((main (function (int 32) (int 32) (pointer (pointer (int 8)))) "main")
         (puts (function (int 32) (pointer (int 8))) "puts"))
       'hare-llvm:llvm)
(build '((defvar main (lambda () 0)))
       '((main (function (int 32)) "main"))
       'hare-llvm:llvm)
(build '((defvar main (lambda (argc argv) (id argc)))
         (defvar id (lambda (x) x)))
       '((main
          (function (int 32) (int 32) (pointer (pointer (int 8))))
          "main"))
       'hare-llvm:llvm)
(build '((defvar main (lambda (argc argv) (iscntrl argc)))
         (declaim (variable iscntrl)))
       '((iscntrl (function (int 32) (int 32)) "iscntrl")
         (main
          (function (int 32) (int 32) (pointer (pointer (int 8))))
          "main"))
       'hare-llvm:llvm)
|#
