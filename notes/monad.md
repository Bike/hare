Since we're doing all this ADT nonsense it would be nice to support Haskell's "do" notation. I think that would unambiguously be better than `seq`.

In a (now-)conventional functional programming system we can define functions `ret : T -> M T` and `bind : M T -> (T -> M U) -> M U`. M is a type constructor that, with these definitions, works as a "monad".

So for example we could define a `Maybe` monad with `(defadt maybe (t) (nothing) (just t))`, with `(defvar ret (lambda (x) (just x)))` and `(defvar bind (lambda (v f) (case v ((nothing) v) ((just x) (f x)))))`. (Ignore how the ad hoc polymorphism of these functions is done for now.) Now `bind` lets us chain compuations that may fail - a computation that fails has `nothing` returned immediately rather than calling the rest of the functions in the chain.

Or we can define the list monad:

```
(defadt list (t) (null) (cons t (list t)))
(defvar append
  (lambda (L1 L2)
    (case L1
      ((null) L2)
      ((cons E NL1) (cons E (append NL1 L2))))))
(defvar ret (lambda (x) (cons x (null))))
(defvar bind
  (lambda (lst f)
    (case lst
      ((null) lst)
      ((cons x xs) (append (f x) (bind xs f))))))
```

Now `bind` lets us chain computations that return several results. Say `sqrt` returns a list of the positive root and the negative root. Then `(bind (ret 1) (lambda (x) (bind (bind x sqrt) sqrt)))` returns `(1 -1 1 -1)` or so. Great.

In a functional context this works well because we have closures. The second example shows the importance of them.

Hare is stupid (has a simplistic runtime) and does not have closures, so `bind` gets a lot less useful (though could still be defined). But the concept of them is still useful as a macro. We could have

```
(defmacro do (form &body forms)
  (if (null forms)
      form
      (let ((vs (gensym)))
        (multiple-value-bind (var form)
            (if (and (consp form) (consp (cdr form)) (consp (cddr form))
                     (null (cdddr form)) (eq (car form) '<-))
                ;; (<- var form)
                (values (second form) (third form))
                (values (gensym) form))
          `(let ((,vs ,form))
             (case ,vs
               ((nothing) (nothing))
               ((just ,var) (do ,@forms))))))))
```

Now we have `bind` for the more limited case where the operands are all immediately apparent closures, essentially - which is common enough in Haskell that there's a special syntax for it, so I would guess that this is worthwhile.

This is not a problem - we can do it with Hare as defined even at this primitive time. The problem comes when we want to reintroduce ad hoc polymorphism. We would like to use a different `do` macro depending on the derived type of its first parameter - which would determine what monad is involved. Conceptually this is no issue since type inference doesn't care if evaluations are eager or not.

We could break this macro into a general part and a specific part:
