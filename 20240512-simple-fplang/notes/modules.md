# Modules

What *is* a module?

My first answer was: a collection of expressions bound to names.

```
val id = \x. x;

val false = \x, y. x;
val true = \x, y. y;
val ifte = \b, t, e. b(e, t);

val zero = \s, z. z;
val suc = \n. \s, z. s(n(s, z));

val three = suc(suc(suc(zero)));
val alsoThree = 3;
val alsoAlsoThree = 2 + 1;
val alsoAlsoAlsoThree = three(\n. n + 1, 0);
```

If I allow arbitrary expressions, then I have to decide how to compile things like `val alsoAlsoThree = 2 + 1;`.
The named expression is reducible, that is, there might be some computation required to produce the promised value.
Here are some options:

* Normalise all module's bindings at runtime when that module is first "loaded"

  How to handle a module that's imported many times in a system?
  How to deal with module sealing?
  What about the performance cost?

* Normalise all module's bindings at compile time

  How to handle side-effects like performing input and output?

* Every binding is actually a computation that produces its right-hand side

  What about the performance cost of repeated evaluation?

* Every binding is actually a computation that produces its right-hand side and memoizes the result

  What about side-effects?

All the answers I come up with lead to more questions, which feels like the road to accidental complexity.

What about syntactically ruling out non-normalised expressions?
`val alsoAlsoThree = 2 + 1;` is not allowed.
This is more in the spirit of the project; a restriction that's easy to implement while still allowing most of the programs a developer would write[^1].
A module is a collection of *values* (normal forms) bound to names.

[^1]: This is a hypothesis that will be tested by actually using the language.
