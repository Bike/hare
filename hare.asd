(defsystem "hare"
  :version "0.0.0"
  :license "Public Domain / Unlicense"
  :description "Programming language."
  :author "Bike <aeshtaer@gmail.com>"
  :components
  ((:file "packages")
   (:file "conditions" :depends-on ("packages"))
   (:file "env" :depends-on ("conditions" "packages"))
   (:file "ground" :depends-on ("env" "packages"))
   (:file "type-env" :depends-on ("conditions" "packages"))
   (:file "type" :depends-on ("type-env" "packages"))
   (:file "polytype" :depends-on ("type" "packages"))
   (:file "type-subst" :depends-on ("polytype" "type" "packages"))
   (:file "adt" :depends-on ("type-subst" "type" "packages"))
   (:file "parse-type" :depends-on ("type" "type-env" "packages"))
   (:file "ast" :depends-on ("type" "env" "packages"))
   (:file "literal" :depends-on ("type-env" "env" "packages"))
   (:file "generate-ast" :depends-on ("literal" "type" "env" "packages"))
   (:file "phase0" :depends-on ("type" "env" "packages"))
   (:file "phase0parse" :depends-on ("phase0" "ground" "env" "type-env" "type"
                                              "literal" "packages"))
   (:file "infer" :depends-on ("type" "literal" "ast" "packages"))
   (:file "module" :depends-on ("parse-type" "infer" "literal" "env"
                                             "packages"))
   (:file "manifest" :depends-on ("module" "infer" "packages"))))
