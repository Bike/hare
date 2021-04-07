I want ad hoc polymorphism of some kind, because it is very convenient, but it introduces problems.

Ad hoc polymorphism is where different monotypings of "the same" polymorphic function (or whatever other object) have totally different code (contents). This is e.g. C++ overloading.

Hare would naturally use early binding rather than late (again, like C++). An easy way to think of this is that each distinct definition of a polymorphic name is actually a separate name, disambiguated by type. For example,

```
(defvar id 328)
(defvar id (lambda (x) x))
(defvar f (lambda () (id (+ id 7))))
```

Here we could intuitively expect `(f)` to be 335 rather than an error. With names separated, this would be

```
(defvar idi 328)
(defvar idf (lambda (x) x))
(defvar f (lambda () (idf (+ idi 7))))
```

which is obviously unambiguous.

My original idea was just to have all global objects be totally polymorphic. This does not work. Consider

```
(defvar id (lambda (x) x))
(defvar f (lambda (y) (id (id y))))
```

Intuitively `f` has the same type as `id`, but with totally unrestrained polymorphism this cannot be derived: the outer `id` has its type instantiated as `a -> b` say, and the inner's as 'c -> d'. Then you can unify `a` with `d`, but that's it: `f` has type `c -> b`, both being type variables, which is totally unhelpful.

The next step is to make the instantiator at least somewhat aware of the actually apparent definitions. In the first example, if this is the whole program, `id` is either a function or an integer but not anything else, and the uses of `id` as a callee and as an argument to `+` disambiguate things perfectly well.

It is not however 100% obvious how to do this. For example, is polymorphic recursion allowed? Then while deriving the type of a function it doesn't know its own type - since any use of the name could refer to some other implementation.
