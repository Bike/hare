I've been thinking of Hare as totally lacking closures, but this may be unnecessarily limiting.

In a language like Lisp, there's pretty much one function type, as far as the runtime is concerned, a closure. It'll be something like a structure consisting of a pointer to a vector and a pointer to a block of code (function). They might support other kinds of calls as an optimization, but this is the norm.

In a language like C there are a variety of function types, but they all basically mean blocks of code. There is one function call operator and it calls these blocks by arranging the arguments and jumping. The function has no storage per se. You can't allocate a new closure which has the same underlying function but different closed over variables/values.

C++ has closures but in the C++ way, they're high enough level to be complicated and not low level enough to be usefully picked apart. I don't want to do that.

There are two basic ways to do closures. One, have a strucure with a function and whatever closed over data. This could be a pointer to the enclosing stack frame or could just be data directly. Two, if the closure is known to be called only at a given depth, have a regular function that crawlls up the stack to find variables; actually I suppose in general you could have it search up the stack until it finds a frame corresponding to the encloser e.g. by checking function names in DWARF etc. The second is kind of exotic and dependent on stack interrogation, so I'm going to put it on the back burner. So, simply, a closure is one of these structures.

Calling a closure is not like calling a function. The caller cannot just jump; it needs to grab the closed over data and pass it to the function. So the same code cannot be used to call a closure and a function. In Hare terms, closures and functions are distinct types. But it would still be nice to be able to call them with the same syntax.

Here's the plan:

1. Define a built in function `call`. This is implicitly used in all calls, i.e. `(f x)` and `(call f x)` are identical provided `f` is not a macro etc.
2. `call` has built-in definitions for any function (arrow) type and appropriate combination of arguments. (Templating will be required here but I haven't decided how it ought to work.)
3. A closure library exists. This defines a closure structure. The closure has a slot for a function (of any arrow type) and an arbitrary set of data. The library includes a definition of `call` for all possible closure types.
4. A compatibility layer is included to assist in passing closures to functions with C style callbacks (i.e., a `whatever (*)(void*)` and `void*`). This is a macro that will define a function that takes a closure, separates it out into a function (possibly this must also be defined) and void*, and calls the original function with that.

For example say we want to call an `void* f(void* (callback*)(void*), void*)`, which in Hare terms has type `(<- i8* (* (<- i8* i8*)) i8*)` To pass a closure holding an `i8*`, we can just do `(defun f (closure) (f (closure-fun closure) (closure-data closure)))` essentially. For a closure over two `i32`s it would be more involved, e.g.

```
(defun f_aux (c) ; (<- i8* i8*)
  (case (bytescast c)
    ((closure cptr i1 i2) (cptr i1 i2))))
(defun f (closure) (f f_aux (castbytes closure)))
```

Essentially we smuggle the real closure into a void*, and the real callback function just calls it.
