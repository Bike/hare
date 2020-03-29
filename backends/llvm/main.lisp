(in-package #:hare-llvm)

(defun single-module (forms &optional (to-file "/tmp/test.bc"))
  (hare::with-type-cache ()
    (let* ((module (hare::parse-module forms))
           (exports (hare::exports module))
           (export-map (loop for (var type) in exports
                             collect (cons var type)))
           (needed (hare::manifest module export-map)))
      (with-module ("test")
        (bindings->llvm needed exports (hare::externs module))
        (llvm:verify-module *module*)
        (llvm:write-bitcode-to-file *module* to-file)))))

#| ;; e.g.
(single-module '((defvar main (lambda () 0))
                 (export main (function (hare:int 32)) "main")))
(single-module '((defvar main (lambda (argc argv) (id argc)))
                 (export main (function (hare:int 32)
                               (hare:int 32) (hare:pointer
                                              (hare:pointer (hare:int 8))))
                  "main")
                 (defvar id (lambda (x) x))))
(single-module '((defvar main (lambda (argc argv) (iscntrl argc)))
                 (export main (function (hare:int 32)
                               (hare:int 32) (hare:pointer
                                              (hare:pointer (hare:int 8))))
                  "main")
                 (hare::extern iscntrl (function (hare:int 32) (hare:int 32))
                  "iscntrl")))
|#
