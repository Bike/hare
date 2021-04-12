(defpackage #:hare.ast
  (:use #:cl)
  (:shadow #:type #:case #:variable #:integer)
  (:export #:type)
  (:export #:variable #:make-variable)
  (:export #:initializer
           #:mapnil-initializer
           #:integer-initializer #:value
           #:variable-initializer
           #:constructor-initializer #:constructor #:fields
           #:undef-initializer #:undef
           #:lambda-initializer #:params #:body
           #:array-initializer #:elements)
  (:export #:ast
           #:mapnil-ast #:map-ast
           #:seq #:asts
           #:call #:callee #:args
           #:literal #:initializer
           #:reference #:name
           #:bind
           #:case #:adt-def #:clauses #:case!p
           #:case-clause #:variables
           #:construct))

(defpackage #:hare
  (:use #:cl)
  (:shadow #:type #:case #:variable)
  (:local-nicknames (#:ast #:hare.ast))
  (:export #:with-type-cache
           #:type
           #:int #:int-type-length
           #:pointer #:pointer-type-underlying
           #:fun #:fun-return #:parameters
           #:arrayt #:arrayt-element-type
           #:adt-def #:name #:tvars #:constructors #:members
           #:adt #:adt-args #:constructor
           #:subst-type)
  (:export #:variable #:initializer)
  (:export #:unify))
