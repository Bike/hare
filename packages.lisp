(defpackage #:hare
  (:use #:cl)
  (:shadow #:type #:case #:variable)
  (:export #:with-type-cache
           #:int #:int-type-length
           #:pointer #:pointer-type-underlying
           #:fun #:fun-return #:parameters
           #:arrayt :arrayt-element-type
           #:adt-def #:name #:tvars #:constructors #:members
           #:adt #:adt-args
           #:subst-type)
  (:export #:ast
           #:seq #:asts
           #:call #:callee #:args
           #:literal #:initializer
           #:reference #:variable
           #:bind #:value #:body
           #:case #:construct)
  (:export #:unify))
