(defsystem :hare-llvm
  :version "0.0.0"
  :license "Public Domain / Unlicense"
  :description "LLVM codegen backend for Hare"
  :author "Bike <aeshtaer@gmail.com>"
  :depends-on (:llvm :hare)
  :components
  ((:module "backends"
    :components
    ((:module "llvm"
      :components
      ((:file "package")
       (:file "type" :depends-on ("package"))
       (:file "translate" :depends-on ("type" "package"))
       #+(or)
       (:file "main" :depends-on ("translate" "package"))))))))
