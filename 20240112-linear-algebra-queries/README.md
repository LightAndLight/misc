# Linear algebra queries

*2024-01-12*

Linear algebra on semirings generalises some (maybe all) of relational algebra.

Represent a binary relation `R \in A x B` as a matrix of booleans
by assigning a column to each element of `A` and a row to each element of `B`,
writing `true` in element `i, j` when `a_j` is related to `b_i` and false otherwise.

```
    a_1 a_2 ... a_n
b_1 x   x   ... x
b_2 x   x   ... x
... .   .   ... .
b_m x   x   ... x
```

Under the semiring `(Bool, false, ∨, true, ∧)`, the identity matrices are the identity relations and matrix multiplication is relation composition.
Combining two of these matrices by element-wise `∧` gives the intersection of two relations,
and by element-wise `∨` gives the union.
The transpose of a matrix is the converse of its relation. 
This equivalence means that these boolean matrices are an [allegory](https://en.wikipedia.org/wiki/Allegory_(mathematics)), so we can do relational programming with them.

There is a functor from the category of boolean matrices to the category of natural number matrices.
It take `false` to `0` and `true` to `1`, and replaces the boolean semiring with `(Nat, 0, +, 1, *)`.
It has a left inverse that takes `0` to `false` and positive numbers to `true`.
Element-wise multiplication of natural number matrices is no longer idempotent,
so this category is technically not an allegory.
But the operation "looks" like relation intersection from the perspective of the nat-to-bool functor.

The lack of idempotence for `+` and `*` is good for quantitative analysis.
Where `∨` and `∧` propagate set membership, `+` and `*` propagate *weights* or *occurrences*.
For example, the row vector with a column for each element of A, set to 1, will add together all the columns any matrix composed on the right.
A column vector with a row for each element of A, set to 0 or 1, represents a subset of A,
and composing on the right of the aforementioned row vector (the dot product!) returns the size of the subset.
If the matrix elements were booleans, then the result would be a boolean indicating whether or not the set was empty.
The natural number semiring keeps more information.

If we have a column vector with a row for each element of A, then left-composing the all-ones
row vector (dot-product) under the max-times semiring `(Nat, -∞, max, 1, *)` gives the maximum value present in the column vector.
Under the min-times semiring `(Nat, ∞, min, 1, *)` we get the minimum value.

References:

* [A linear algebra approach to OLAP](https://repositorio.inesctec.pt/server/api/core/bitstreams/a533c8a1-0d69-4517-87e9-060503d3dc48/content)
* [Towards a linear algebra semantics for SQL](https://www.di.uminho.pt/~jno/ps/infoblender16sl.pdf)
* [Fun with Semirings](https://web.archive.org/web/20160305153614id_/http://www.cl.cam.ac.uk/~sd601/papers/semirings.pdf)
