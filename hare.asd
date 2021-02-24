(defsystem "hare"
  :version "0.0.0"
  :license "Public Domain / Unlicense"
  :description "Programming language."
  :author "Bike <aeshtaer@gmail.com>"
  :components
  ((:file "packages")
   (:file "conditions" :depends-on ("packages"))
   (:file "env" :depends-on ("conditions" "packages"))
   (:file "type-env" :depends-on ("conditions" "packages"))
   (:file "type" :depends-on ("type-env" "packages"))
   (:file "parse-type" :depends-on ("type" "type-env" "packages"))
   (:file "ast" :depends-on ("type" "env" "packages"))
   (:file "literal" :depends-on ("type-env" "env" "packages"))
   (:file "generate-ast" :depends-on ("literal" "type" "env" "packages"))
   (:file "infer" :depends-on ("type" "literal" "ast" "packages"))
   (:file "module" :depends-on ("parse-type" "literal" "env" "packages"))))
