A programming language. It is not usable yet.

The premise is a language with a Hindley-Milner type system (constructors, polytypes, inference) suitable for standalone programs. So like ML if ML was uninteractive and involved a lot more pointer arithmetic.

By "standalone programs" I mean this thing is supposed to fill the same kind of niche as C. You will be able to compile it to binaries, in whatever format the OS is used to. The binaries will export symbols and be linkable with other Hare, C, or whatever binaries.

Grammar
-------

In flux.

module := definition*
definition := defconstant | defvar | defadt
type := tvar | (INT integer) | (POINTER type) | (FUNCTION type type*) | (ARRAY type) |
        (adt-name type*)
tvar := symbol
adt-name := symbol
defadt := (DEFADT adt-name (tvar*) (constructor type*)*)
defconstant := (DEFCONSTANT name initializer)
defvar := (DEFVAR name [initializer])
name := symbol
constructor := symbol
initializer := symbol | integer | UNDEF | (ARRAY initializer*) |
               (ARRAYN integer) | (BYTES initializer) | (constructor initializer*) |
               (LAMBDA (name*) form*)
form := name | combination | literal
literal := name | (constructor literal*)
combination := (LET (name form) form*) | (IF form form form) | (SEQ form*) |
               (CASE form ((constructor name*) form*)*) |
               (CASE! form ((constructor name*) form*)*) |
               (WITH (name [initializer]) form*) |
               call
call := (form form*)

See literals.lisp, types.lisp, and ast.lisp for explanations of the literal/initializer, type, and evaluation semantics respectively.

Compilation semantics
---------------------

C defines things in terms of translation phases so let's do that.

1. You start out with ASTs of some kind. These can be translated to a "module". A module contains polymorphic definitions, as well as things only of interest to a compiler, such as constants, macro definitions, type definitions, and extern declarations (i.e. a notice that a given name will eventually be linked in as having some type).
2. A module can be translated into an "object", like a binary. An object does not (in general) have macro definitions or constants. Everything is monotyped. This means any given polymorphic definition has been "manifested" as one or more monomorphic definitions. Definitions are distinguished by some form of name mangling, unless I can figure out how to make a linker behave pretty damn exotically.
3. Objects are linked together etc. ditto C.

A "module" is analogous to a C header, but it can contain structured information rather than text.

Phase 2 essentially strips out defconstant and defadt forms, at least to the point of making them irrelevant. Each defvar defines one (or more) regions of memory accessible through the symbol table.

Text format is not really specified so far. I would especially like to be able to produce an object file without any actual starting text, i.e. producing in-memory AST structures by whatever means. Text is not really an efficient way to deal with programs, and also I don't want to deal with encodings very much.

Macros I haven't thought too much about. Ideally they would be in a more abstract language. Might be good to add another phase for removing any macro definitions written in some other language, so the compiler can focus.
