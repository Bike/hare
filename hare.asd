(defsystem "hare"
  :version "0.0.0"
  :license "Public Domain / Unlicense"
  :description "Programming language."
  :author "Bike <aeshtaer@gmail.com>"
  :components
  ((:file "packages")
   (:file "conditions" :depends-on ("packages"))
   (:file "env" :depends-on ("conditions" "packages"))
   (:file "type" :depends-on ("env" "packages"))
   (:file "ast" :depends-on ("type" "env" "packages"))
   (:file "literal" :depends-on ("env" "packages"))
   (:file "generate-ast" :depends-on ("literal" "type" "env" "packages"))
   (:file "module" :depends-on ("literal" "env" "packages"))))
