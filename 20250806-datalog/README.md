# Datalog

*2025-08-06*

A toy Datalog-inspired language.

## Notes

* Why do people care about Datalog specifically, as opposed to general logic programming languages?

  Datalog is structured so that all programs have a unique fixed point, even recursive ones are badly behaved in other languages.
  For example, `r(X) :- r(X). r(Y).` causes a stack overflow in `gprolog`, but has the unique answer `no` in Datalog.

* Bottom-up versus top-down

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

## References

* Abiteboul, S., Hull, R., & Vianu, V. (1995). Foundations of Databases. Retrieved from http://webdam.inria.fr/Alice/

* Beeri, C., & Ramakrishnan, R. (1987, June). On the power of magic. In Proceedings of the sixth ACM SIGACT-SIGMOD-SIGART symposium on Principles of database systems (pp. 269-284).
