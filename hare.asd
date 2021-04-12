(defsystem "hare"
  :version "0.0.0"
  :license "Public Domain / Unlicense"
  :description "Programming language."
  :author "Bike <aeshtaer@gmail.com>"
  :components
  ((:file "packages")
   (:file "conditions" :depends-on ("packages"))
   (:module "type"
    :depends-on ("packages" "conditions")
    :components ((:file "cache")
                 (:file "type" :depends-on ("cache"))
                 (:file "polytype" :depends-on ("type"))
                 (:file "type-subst" :depends-on ("polytype" "type"))
                 (:file "adt" :depends-on ("type-subst" "type" "cache"))))
   (:module "ast"
    :depends-on ("packages" "type")
    :components ((:file "ast")
                 (:file "literal")))
   (:file "env" :depends-on ("conditions" "packages"))
   (:file "ground" :depends-on ("env" "packages"))
   (:file "type-env" :depends-on ("conditions" "packages"))
   (:file "parse-type" :depends-on ("type" "packages"))
   (:file "generate-ast" :depends-on ("ast" "type" "env" "packages"))
   (:file "phase0" :depends-on ("type" "env" "packages"))
   (:file "phase0parse" :depends-on ("phase0" "ground" "env" "type-env" "type"
                                              "ast" "packages"))
   (:file "infer" :depends-on ("type" "ast" "packages"))
   (:file "module" :depends-on ("parse-type" "infer" "ast" "env" "packages"))
   (:file "manifest" :depends-on ("module" "infer" "packages"))))
