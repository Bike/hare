A programming language. It doesn't work yet.

The premise is a language with a Hindley-Milner type system (constructors, polytypes, inference) suitable for standalone programs. So like ML if ML was uninteractive and involved a lot more pointer arithmetic.

By "standalone programs" I mean this thing is supposed to fill the same kind of niche as C. You will be able to compile it to binaries, in whatever format the OS is used to. The binaries will export symbols and be linkable with other Hare, C, or whatever binaries.

Grammar
-------

In flux.

```
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
```

See literals.lisp, types.lisp, and ast.lisp for explanations of the literal/initializer, type, and evaluation semantics respectively.

Compilation semantics
---------------------

C defines things in terms of translation phases so let's do that.

1. You start out with ASTs of some kind. These can be translated to a "module". A module contains polymorphic definitions, as well as things only of interest to a compiler, such as constants, macro definitions, type definitions, and extern declarations (i.e. a notice that a given name will eventually be linked in as having some type).
2. A module can be translated into an "object", like a binary. An object does not (in general) have macro definitions or constants. Everything is monotyped. This means any given polymorphic definition has been "instantiated" as one or more monomorphic definitions. Definitions are distinguished by some form of name mangling, unless I can figure out how to make a linker behave pretty damn exotically.
3. Objects are linked together etc. ditto C.

A "module" is analogous to a C header, but it can contain structured information rather than text.

Phase 2 essentially strips out defconstant and defadt forms, at least to the point of making them irrelevant. Each defvar defines one (or more) regions of memory accessible through the symbol table.

Text format is not really specified so far. I would especially like to be able to produce an object file without any actual starting text, i.e. producing in-memory AST structures by whatever means. Text is not really an efficient way to deal with programs, and also I don't want to deal with encodings very much.

Macros I haven't thought too much about. Ideally they would be in a more abstract language. Might be good to add another phase for removing any macro definitions written in some other language, so the compiler can focus.

To-do list
----------

Ideally it should work.

Also, add:

1. extern declarations: this pointer will be linked in and have this type
2. compiler hints concerning functions, such as: inline
3. hints concerning variables, such as: restrict?
3. hints concerning representation of types, such as: tagged pointers, alignment, packing
4. ABI definition
5. atomic operations
6. continuations

Things I would like to work:

1. the language
2. (defadt immediate (o) (fixnum (int 64)) (object (pointer o))) = tagged pointer
3. (defadt object () (object (array byte) header)) = pointers to objects are pointers to the bytes, since they're more often used, and the machine subtracts to get the header

Language design questions
-------------------------

Should LET and WITH be polymorphic? Going off of ML, the answer is yes for LET at least, but given the machine orientation here some consequences may be strange.

To implement this, any polymorphic binding would essentially be expanded into multiple monomorphic bindings, just like for the top level. But this has some weird consequences: any LET value form would be executed multiple times, including any side effects; and any WITH would allocate storage for every monomorphic type involved, which may be more than the user expects. Given this and the fact that without local closures polymorphic locals lose a lot of their appeal (I think), I'm going to hold off on bothering to implement polymorphism here. I can imagine it being useful in a few other situations, like for different integer types, but again imma wait until I can try it in a working system.

Object representation
---------------------

In C and C++, any object put in storage (memory) can be read out as bytes. The number of bytes is defined by sizeof of the type and the layout is in large part (but not completely, there's "empty base optimization" and stuff) defined by the type. I think this is abstracting memory overmuch and is needlessly restrictive: there's no reason not to allow writing the same object to memory (and reading it back) in multiple layouts.

I think memory should be kind of an accessory to the language, basically. A very important one, but nonetheless shouldn't be part of the core semantics. C conflates variables with storange and it confuses the hell out of everyone. And some types may not correspond to storage at all usually. If you return a bool/i1, you can just keep it in the carry flag in the ABI, then let the caller's register allocation take care of preserving it if need be.

Here's how I think this would work. The user can define "representations" for a type. Also any type has a default implementation-defined representation, because doing this manually for every type would kind of suck. A representation defines how the Hare-semantic fields correspond to bytes in memory. All pointer types are annotated with a representation, and the load and store operations are compiled to like, do that. Two pointer types with the same underlying type but different representations would be unrelated as far as inference is concerned.

Some useful representations might be built in, e.g. integer types might have little-endian and big-endian representations available.

Now, what does a representation consist of? Our types consist of integers, floats at some point, pointers, functions, arrays, and ADTs.

Alignment is either a property of the object representations or of the representations of pointers to the objects. I guess having it with the pointer makes more sense: you could have the same sequence of bytes (object representation) aligned or unaligned, for example.

* Integers as mentioned have endianness. I think user-specifying byte layout might be a little much in general, but maybe? Let's just say integer representations can't be customized, for now.
* Ditto floats.
* Pointers would include alignment information as mentioned above. It should also be possible to have tags in low bits thus freed by the alignment, and possibly high bits depending on paging??
* Arrays... I mean, the point is it's just contiguous of the objects in it. So you'd choose the representation of the underlying type. And I guess maybe padding and alignment stuff.

ADT representation includes enough complexity that actually let's drop the list thing.

At first I thought you'd basically start by expressing a representation for each member but it's a little more complicated than that. With that we can have padding and alignment crap specified so you know that's a start. Extra note, we can have empty members (for an ADT with one 0-ary constructor), which apparently C++ doesn't usually allow without no_unique_address annotations.

It gets more complicated with tagged unions. First, we can have a representation that only includes certain of the ADT's constructors, i.e. the object is guaranteed to not be a constructor not in the representation. Second I guess we could let the user specify what values for tags or how big the tag is and stuff like that, treat it like a field.

But eliding tags could have some weird consequences. Start with arrays. Why not allow a representation for an array that includes a header with the tag, and then the elements of the array don't include a tag (and have uniformly the same constructor)? Or hell, a header with PART OF the tag and then the array elements have a shorter tag?? I guess having a header makes this "array" an ADT itself with a flexible array member, so we can still stick with talking about ADTs...

So we'd have like (defadt thing () (a ...) (b ...) (c ...) (d ...)), then (defadt my-array () (my-array (int 2) (array thing))), (defrepresentation my-array () default (array (thing sans-tag))), and then (defvar my-array-ref (arr index) ...) somehow makes a pointer where its representation of thing has the consturctor chosen from the tag jesus what a rabbit hole

There are questions about pointers too. Can't do case! if the representation doesn't have a tag or the fields don't have tags, because a field no longer has a distinct address.

hell, i just realized representations for type CONSTRUCTORS are gonna make things even more confusing. like having an adt with type arguments. hoo boy

TODO
