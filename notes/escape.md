Primitive
---------

Mechanism kind of like setjmp/longjmp, but without the dumb runtime flag check.

Special operator `(let/ec [symbol] [body...]) => T`

Binds a new variable named by SYMBOL to a value of type `(Escape T)`, `T` being the return type of the form. The body forms are evaluated, and in normal execution (i.e. if `escape` is not called with the escape object) the final form's value will be the value of the let/ec form as a whole, similar to the sequence operator.

The escape may be used with the `escape` function below.

Function `(escape (Escape T) T) => [does not return]`

Performs an immediate escape from the current computation. `escape` does not return; instead, a call to `escape` results in the `let/ec` execution that created the escape object exiting.

For example, `(let/ec e (f) (escape e (g)) (h))` is equivalent to `(sequence (f) (g))`. The computation escapes before `h` is called.

The association between the `let/ec` and the escape object is based on the actual object. For example, consider

```
(defvar *escape*)
(defun aux (n flag)
  (if (=? n 0)
      (escape (! *escape*) #inert)
      (let/ec e
        (if flag #inert (set! *escape* e))
        (aux (- n 1) #t)
        (other)
        #inert)))
(defun test (n) (aux n #f))
```

Calling `(test n)` will result in n+1 nested recursive calls to `aux`, n of which establish an escape. However, only the outermost call saves its escape in the global `*escape*` variable. As such, the final call to `escape` exits from this outermost call, and so `other` is only called once for any n greater than zero.

Any given `let/ec` execution can only return once, either normally or through `escape`. If `escape` is called with an escape object associated with a `let/ec` that has already returned, the behavior is undefined.

Note that the types of the normal return value of the `let/ec`, as well as all calls to `escape` for it, must agree.

Type forall T. (Escape T)

Type of objects created and used by the `let/ec` operator and `escape` function, representing the possibility of returning a value of type T. Escapes are sized types [i.e. they are first class objects, not just in memory like arrays, and can be stored and loaded]. Beyond these properties no particular behavior or representation of these objects is defined.

Unwinding
---------

The above is actually fairly simple. LLVM doesn't really support it very well but conceptually there is no problem. Where there is a problem is implementing things like `unwind-protect` or `dynamic-wind`. These are very important operators for serious programming with continuations, but they entail a pretty sophisticated runtime system. `escape` can no longer just be a jump and maybe a stack pointer change; it has to interrogate a data structure (the stack) and execute arbitrary code.

LLVM's attempt at this, the catchpads and landingpads, is just terrible. Totally built on C++. No attempt at theoretical elegance. Doesn't even really allow you to use the general Itanium system which nominally should allow interoperation with non-C++ languages with different semantics. That said, the "invoke" instructions are probably part of a good IR representation.

I think it's important to remember that the call stack and really the entire concept of functions is an abstraction. The stack is a data structure. Presuming for a moment no frame pointer elimination and no stack frames growing during execution, we have

```
(defadt frame
  (top)
  (child (pointer code) (pointer frame) (array local-var)))
```

so to speak. It's just a linked list. In systems with indefinite extent continuations it may literally be a regular old heap-allocated linked list.

Now the unwinder can easily traverse this data structure. To know where to stop it needs to be able to peek into the Escape a little bit to see some marker, like say the frame pointer. This is enough to perform the useful action of checking that an escape is still valid. But to do more it now needs additional information, about `unwind-protect` actions. We can just add another field:

```
(defadt frame
  (top)
  (child (pointer code) (pointer frame) (maybe cleanup-thunk) (array local-var)))
```

Then the unwinder runtime just executes any cleanup-thunks between it and its destination.

In C++ the situation is more complex. There are cleanup thunks which are basically covered by this procedure, but additionally there are dynamic exception handlers which must test an exception object against a variety of types, etc. Additionally C++'s `throw;` statement implies a dynamic stack of "current exceptions" which may be accessed from anywhere. These are major complications not inherent to the process, but it would be useful to have something that can implement C++.

More simply to think about as far as I've concerned, the Common Lisp condition system might be interesting to consider in these terms. Generally Lisp implementations do not actually use the unwinder in the condition system itself per se: functions like `signal` just consult a dynamically maintained list of handler functions, and _those_ may do some unwinding - or not! Restarts, similarly, are usually functions under the hood and don't necessarily do any control transferring. But we can consider what an unwinding runtime might need to support in order to implement a more "direct" condition system.

We can do all of these things if we can place information on the call stack and read it back later. This information may be arbitrary. For merely `unwind-protect` it is just the possibility of a thunk. For C++ exceptions it would be a mapping of exception types to points to jump to, or something. Similarly for control-transferring restarts.

The major problem is that these things are not composable. Every function in the program, including ones that don't particularly give a damn about unwinding (or may give a damn about one kind but not another kind) needs to have a call frame with all information accessible, or at least a defined lack of information. For example in a system that had both `unwind-protect` and C++ exceptions, every frame would need both the thunk indicator and the exception mapping, or at least markings saying "nothing here" for both. Not just every frame the programmer controls, either - any frame that any code could possibly unwind through!

Here's the crazy solution I thought up just now. Say we do fix the frame format, but only a little. We add a word to each frame, which is a possibly null pointer. If it's null, that means this frame has nothing of interest to any unwinder. Otherwise, it's a pointer to some object which represents the nature of the frame. To get any information about a frame, the unwinder calls a generic function with two arguments, this object, and possibly the frame itself. If the generic function has a method it returns what the unwinder wants to know, possibly by reading this information from the frame. Otherwise the systems are orthogonal and the unwinder stops bothering with this frame. The frame tag object is probably just for dispatching and doesn't have any information in it - many different frames can share the same object. It's sort of like an Itanium personality function?

In any case, this is pretty much too complicated to be a standard part of Hare - I'm just wondering what Hare might have to support it. Escapes are simple enough though.
