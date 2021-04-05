(in-package #:hare-llvm)

(defun process-particulars (particulars env tyenv)
  (loop for (name tyexpr c-name) in particulars
        for var = (hare:variable (hare::lookup name env))
        for ty = (hare::parse-type tyexpr tyenv)
        collect (if c-name (list var ty c-name) (list var ty))))

(defun single-module (exprs particulars &optional (to-file "/tmp/test.bc"))
  (hare:with-type-cache ()
    (let* ((prem (hare::parse-pre-module exprs))
           (env (hare::environment prem))
           (tyenv (hare::type-env prem))
           (mod (hare::module prem))
           (particulars (process-particulars particulars env tyenv))
           (manifest (hare::manifest mod particulars)))
      (with-module ("test")
        (translate manifest)
        (llvm:verify-module *module*)
        (llvm:write-bitcode-to-file *module* to-file)))))

#| ;; e.g.
(single-module '((defvar main (lambda () 0)))
               '((main (function (int 32)) "main")))
(single-module '((defvar main (lambda (argc argv) (id argc)))
                 (defvar id (lambda (x) x)))
               '((main
                  (function (int 32) (int 32) (pointer (pointer (int 8))))
                  "main")))
(single-module '((defvar main (lambda (argc argv) (iscntrl argc))))
               '((main
                  (function (int 32) (int 32) (pointer (pointer (int 8))))
                  "main")
                 (iscntrl (function (int 32) (int 32)) "iscntrl")))
|#
