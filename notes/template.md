I have had a revelation about the C++ template system. It's still messy crap, but I understand its purpose now.

Macros are great, but they define single things. Take the example of a tuple type constructor. It's easy to write the definition of a tuple type for a fixed number of members: `(defadt tuple2 (a b) (t2 a b))` for instance. And with a macro you can define any number of these you want, tuple0 and up - **as long as that number is finite**. What templates let you do is define an infinite number.

I mean, specifically template parameter pack whatevers. But still.

Resolving ad hoc polymorphism in the presence of variadic templates should just be a matter of instantiating the template for the given number of arguments, which I think should always be apparent from context.

Some things C++ does with template (parameter pack)s, that complicate the template system, are I think better handled with macros. cppreference gives an example

```c++
template<typename...> struct Tuple {};
template<typename T1, typename T2> struct Pair {};
 
template<class ...Args1> struct zip {
    template<class ...Args2> struct with {
        typedef Tuple<Pair<Args1, Args2>...> type;
//        Pair<Args1, Args2>... is the pack expansion
//        Pair<Args1, Args2> is the pattern
    };
};

typedef zip<short, int>::with<unsigned short, unsigned>::type T1;
// Pair<Args1, Args2>... expands to
// Pair<short, unsigned short>, Pair<int, unsigned int> 
// T1 is Tuple<Pair<short, unsigned short>, Pair<int, unsigned>>
 
typedef zip<short>::with<unsigned short, unsigned>::type T2;
// error: pack expansion contains parameter packs of different lengths
```

But you could define zip/with with a macro that expands two lists of types into a tuple of pairs of the types. There's no need to give the template an actual identity as a type in itself.

Example variadic function made up of a simple template

```
(defadt %closure (ret data . params)
  (closure (<- ret data . params) data))
(deftype closure (ret data . params)
  `(%closure ,ret (tuple ,@data) ,@params))
(defun call (closure . args)
  ;; APPLY is not actually a function so much as a local macro that expands
  ;; into (call f data arg1 arg2) for however many args.
  (case closure ((closure f data) (apply f data args))))
```

A general template mechanism could work as follows. Add a top level operator called `deftmacro` or something. This defines something that is basically a macro, i.e. a source to source function, but with the special property that all of its parameters are forms. This restricts what it can do - it can't define syntax, really - but means that type inference can treat applications of it the same way it treats calls. It's like a lazy function, except it doesn't actually exist at runtime. Also, it allows ad hoc polymorphism.

A major advantage here is that it subsumes (some) things Haskell can do with laziness. For example we could define

```
(deftmacro bind (m1 m2) (case ,m1 ((nothing) (nothing)) ((just %internal) ,m2)))
```

and then

```
(defmacro do (form . forms)
  (if (null forms)
      form
      (multiple-value-bind (var form)
          (if (and (consp form) (consp (cdr form)) (consp (cddr form))
                   (null (cdddr form)) (eq (car form) '<-))
              (values (second form) (third form))
              (values (gensym) form))
        `(bind ,form (let ((,var %internal)) (do ,@forms))))))
```

Now we have a do syntax as in monad.md, but specializable by type by defining a new definition of `bind`. Of course the use of `%internal` is very ugly; something like `syntax-rules` might work - the point is it's a macro but the implementation understands what parts of the macro form are subforms.

Note that this doesn't obviate the need for variadic function templates. Those actually define an infinite number of functions - actual functions with a place in memory etc that can be used in places other than the car of a form.
