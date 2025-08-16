# Datalog

*2025-08-06*

A toy Datalog-inspired language.

## Notes

### Why do people care about Datalog specifically, as opposed to general logic programming languages?

Datalog is structured so that all programs have a unique fixed point, even recursive ones are badly behaved in other languages.
For example, `r(X) :- r(X). r(Y).` causes a stack overflow in `gprolog`, but has the unique answer `no` in Datalog.

### Bottom-up versus top-down

```prolog
path(X, Y) :- edge(X, Y).
path(X, Z) :- edge(X, Y), path(Y, Z).
```

Bottom-up evaluation incrementally derives new facts that are consequences of known facts.
It's a bulk approach, deducing the contents of new relations from existing ones.
The bottom-up evaluation of the above program will result in a complete `path` relation w.r.t. the present `edge` facts.
These deduced relations are like [SQL views](https://en.wikipedia.org/wiki/View_(SQL)).

Bottom-up evaluation is an inefficient way to answer queries.
When I ask for `path(X, 'a')`, bottom-up evaluation would deduce the entire `path` relation in the process of answering my question,
which includes all the paths that don't end in `'a'`.
When I think about queries, my instinct is to think top-down.
Start with the goal `path(X, 'a')` and recursively reduce to subgoals (`edge(X, 'a')` or `edge(X, Y), path(Y, 'a')`),
producing instantiations of variables when matching facts are found, backtracking so that all subgoals are explored.

The problem I see with the top-down approach is that it leads to the non-termination behaviour from the previous point.
Are there smarter ways to do it that are equivalent to bottom-up evaluation in all situations?

The approach that seems to be favoured by the literature is to rewrite queries so that they have efficient bottom-up solutions.

Bottom-up evaluation relies on facts being efficiently searchable in bulk,
whereas top-down evaluation only requires efficient lookup of individual facts.
I want to use this toy language to ask questions about derivations in my Nix store.

### Functions in rule heads

When you allow functions in rule heads, you can create rules that don't have a (finite) fixed point.
The following program generates the set of natural numbers, so naive/seminaive bottom-up evaluation won't terminate:

```prolog
natural(0).
natural(n + 1) :- natural(n).
```

Even so, some programs with functions-in-heads *do* have finite fixed points.
Consider `why_depends`, which computes transitive paths between nodes:

```prolog
why_depends(X, Y, [X, Y]) :- edge(X, Y)
why_depends(X, Z, [X | Path]) :- edge(X, Y), why_depends(Y, Z, Path)
```

It obviously has a fixed point, because the version with the paths removed also does:

```prolog
why_depends(X, Y) :- edge(X, Y)
why_depends(X, Z) :- edge(X, Y), why_depends(Y, Z)
```

I don't have a precise description of good and bad uses of functions.
I think it has something to do with the way that the production of new facts is constrained by known finite facts.

`r(0). r(X) :- r(X).` has a fixed point (`r = {0}`) even though it's trivially recursive,
because `r(X) :- r(X).` never produces new tuples.
But `r(0). r(X + 1) :- r(X).` has no fixed point,
because each application of `r(X + 1) :- r(X)` produces a new tuple.
If `X` is constrained to a set of finite values by another predicate, e.g.
`r(0). r(X + 1) :- e(X), r(X).` then eventually tuples will be generated that fall outside that finite set,
for which the rule will not fire, breaking the chain of infinite recursion.
`r(_, []). r(X, [X | Y]) :- r(X, Y)`

Maybe a good enough rule is that a self-recursive rule must be "guarded" by a finite predicate.
At least one variable supplied by the recursive call must be matched against a finite set.

That's not quite right. If we have cycles (`edge(x, x)`) then `why_depends` won't terminate.
It'll make increasingly large lists of `[x, x, ..., x]`.
Maybe this hints at a way of detecting runaway recursive rules.
Find an assignment of variables that equates the non-function-valued arguments of the rule head and its recursive call.
In the case of `why_depends` it's `Y = X`, giving `why_depends(X, Z, [X | Path]) :- edge(X, X), why_depends(X, Z, Path)`.
This shows that divergence will be achieved when there exists facts `edge(X, X)` (self-loops).
This could be ruled out by adding a `X != Y` constraint, but this doesn't account for longer paths that loop.
`why_depends(X, Z, [X | Path]) :- edge(X, Y), X != Y, why_depends(X, Z, Path)` passes test I just named,
but we can easy find a loop of length 2 by inlining and following the same process:

```prolog
why_depends(X, Z, [X, Y | Path]) :- edge(X, Y), X != Y, edge(Y, Y1), Y != Y1, why_depends(Y1, Z, Path)
```

`Y1 = X` gives

```prolog
why_depends(X, Z, [X, Y | Path]) :- edge(X, Y), X != Y, edge(Y, X), Y != X, why_depends(X, Z, Path)
```

### Embedding Datalog

There's a variety of research into extending Datalog with aggregations, but my impression so far is that they detract from the elegance of the core theory.
It feels like trying to put a square peg into a round hole.
My pragmatic intuition is to leave Datalog alone, and embed it within a system that can aggregate the results of a Datalog program.

## References

* Abiteboul, S., Hull, R., & Vianu, V. (1995). Foundations of Databases. Retrieved from http://webdam.inria.fr/Alice/

* Beeri, C., & Ramakrishnan, R. (1987, June). On the power of magic. In Proceedings of the sixth ACM SIGACT-SIGMOD-SIGART symposium on Principles of database systems (pp. 269-284).

* Kemp, D. B., & Stuckey, P. J. (1991, December). Semantics of logic programs with aggregates. In International Logic Programming Symposium 1991.

* Ross, K. A., & Sagiv, Y. (1992, July). Monotonic aggregation in deductive databases. In Proceedings of the eleventh ACM SIGACT-SIGMOD-SIGART symposium on Principles of database systems (pp. 114-126).
