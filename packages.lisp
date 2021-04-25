(defpackage #:hare.type
  (:use #:cl)
  (:shadow #:type)
  (:export #:type-cache #:with-type-cache)
  (:export #:type #:type=
           #:int #:int-type-length #:make-int
           #:pointer #:pointer-type-underlying #:make-pointer
           #:fun #:fun-return #:parameters #:make-fun
           #:arrayt #:arrayt-element-type #:make-arrayt
           #:adt-def #:name #:tvars #:constructors #:members #:arity
           #:tvar #:name #:make-tvar
           #:unknown #:expr #:type-env #:transform-unknown
           #:adt #:adt-args #:constructor #:fields #:make-adt #:inert)
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
           #:mapnil-initializer #:map-initializer #:copy-initializer
           #:integer-initializer #:value
           #:variable-initializer
           #:constructor-initializer #:constructor #:fields
           #:undef-initializer #:undef
           #:lambda-initializer #:params #:body
           #:array-initializer #:elements
           #:vla-initializer #:nelements
           #:unknown-initializer #:expr #:environment
           #:transform-unknown-initializer)
  (:export #:ast
           #:mapnil-ast #:map-ast #:copy-ast
           #:seq #:asts
           #:call #:callee #:args
           #:literal
           #:reference #:name
           #:bind #:binding #:bindings
           #:with #:initialize
           #:primitive
           #:case #:adt-def #:clauses #:case!p
           #:case-clause #:variables
           #:construct
           #:unknown #:transform-unknown))

(defpackage #:hare
  (:use #:cl)
  (:local-nicknames (#:ast #:hare.ast) (#:type #:hare.type))
  (:shadow #:type #:variable)
  (:export #:manifest #:backend #:dump)
  (:export #:variable #:initializer #:name)
  (:export #:unify))
