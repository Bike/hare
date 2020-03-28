(in-package #:hare-llvm)

(defun single-module (forms)
  (hare::with-type-cache ()
    (let* ((module (hare::parse-module forms))
           (exports (hare::exports module))
           (export-map (loop for (var type) in exports
                             collect (cons var type)))
           (needed (hare::manifest module export-map)))
      (with-module ("test")
        (bindings->llvm needed exports)
        (llvm:verify-module *module*)
        (llvm:write-bitcode-to-file *module* "/tmp/test.bc")))))

#| ;; e.g.
(single-module '((defvar main (lambda () 0))
                 (export main (function (hare:int 32)) "main")))
|#
