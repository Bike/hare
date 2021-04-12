(defpackage #:hare.type
  (:use #:cl)
  (:shadow #:type)
  (:export #:with-type-cache
           #:type #:type=
           #:int #:int-type-length #:make-int
           #:pointer #:pointer-type-underlying #:make-pointer
           #:fun #:fun-return #:parameters #:make-fun
           #:arrayt #:arrayt-element-type #:make-arrayt
           #:adt-def #:name #:tvars #:constructors #:members #:arity
           #:tvar #:name #:make-tvar
           #:adt #:adt-args #:constructor #:fields #:make-adt)
  (:export #:mapnil-type #:map-type)
  (:export #:schema #:free #:free-in-schema #:instantiate)
  (:export #:tysubst #:empty-tysubst #:make-tysubst #:subst-type
           #:subst-schema #:compose-tysubsts)
  (:export #:instantiate-constructor #:instantiate-adt-def
           #:subst-constructor))

(defpackage #:hare.ast
  (:use #:cl)
  (:local-nicknames (#:type #:hare.type))
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
  (:local-nicknames (#:ast #:hare.ast) (#:type #:hare.type))
  (:shadow #:type #:variable)
  (:export #:variable #:initializer #:name)
  (:export #:unify))
