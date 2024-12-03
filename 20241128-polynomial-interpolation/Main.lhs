> import Data.List (partition)
>
> import Matrix
> import Gauss
> import Plot
> import Plot.Polynomial
> import Polynomial
> import Symbolic

[Interpolation](https://en.wikipedia.org/wiki/Interpolation)

I thought "interpolation" meant "generating points on a line between two given points".
But that's a very narrow understanding of the concept. A more general description is
that interpolation is
[function approximation](https://en.wikipedia.org/wiki/Function_approximation)
based on some known points.
From what I've seen, interpolation finds a function whose graph includes the known points.

Given a single point, `(x, f(x)) = (x_0, y_0)`, the only guess we can make for `f` with any confidence is that it's the function that always returns `y_0`.

> type Point a = (a, a)
>
> interpolate1 :: Num a => Point a -> Polynomial a
> interpolate1 (x_0, y_0) = term y_0 0
>
> interpolate1Plots :: Point Double -> [Plot]
> interpolate1Plots p = [polyPlot $ interpolate1 p, points [p]]

`(x, f x)` is in the graph of `interpolate1 (x, f x)` for all `f`. In other words, we
have `forall f. interpolate1 (x, f x) x == f x`. And all other elements in the graph of
`interpolate1 (x, f x)` are equally valid guesses given what we know about `f`.

By the same logic, given two points `(x_0, y_0)` and `(x_1, y_0)`, the "best" guess for `f` is the line connecting the two points.

> interpolate2 :: Fractional a => (Point a, Point a) -> Polynomial a
> interpolate2 ((x_0, y_0), (x_1, y_1)) = poly (\x -> val m * x + val c)
>   where
>     m = (y_1 - y_0)/(x_1 - x_0)
>
>     -- f(x_0) = y_0 = m*x_0 + c
>     --          y_0 - m*x_0 = c
>     --          y_0 - m*x_0 = c
>     c = y_0 - m*x_0
>
> interpolate2Plots :: (Point Double, Point Double) -> [Plot]
> interpolate2Plots (p1, p2) = [polyPlot $ interpolate2 (p1, p2), points [p1, p2]]

And a parabola is the "best" guess for three points. I found it easy to algebraically solve
for a line given two points, but doing the same thing for a quadratic function seems tedious.

Here is the system of equations, one for each point `(x_i, y_i) for i in 0..2`:

```	
a*x_0^2 + b^x_0 + c = y_0
a*x_1^2 + b^x_1 + c = y_1
a*x_2^2 + b^x_2 + c = y_2
```

This is a system of linear equations of three variables: `a`, `b`, and `c`. Three equations
for three unknowns means that the system should have a unique solution. I'd be
uncomfortable manipulating these equations in ad-hoc ways to find the solution, so instead
I can use [Gaussian elimination](https://en.wikipedia.org/wiki/Gaussian_elimination) to
do it systematically.

> interpolate3 :: (Fractional a, Ord a) => (Point a, Point a, Point a) -> Polynomial a
> interpolate3 ((x_0, y_0), (x_1, y_1), (x_2, y_2)) =
>   poly $ \x -> val a * x^2 + val b * x + val c
>   where
>     m = (3, 3) # [ x^i | x <- [x_0, x_1, x_2], i <- [2, 1, 0]]
>     coeffs = solve m (Column [y_0, y_1, y_2])
>
>     a = coeffs!0
>     b = coeffs!1
>     c = coeffs!2
>
> interpolate3Plots :: (Point Double, Point Double, Point Double) -> [Plot]
> interpolate3Plots (p1, p2, p3) =
>   [polyPlot $ interpolate3 (p1, p2, p3), points [p1, p2, p3]]

This pattern will just keep going:
a polynomial of degree `n` has `n+1` coefficients,
and `n+1` equations of `n+1` variables will always have a unique solution,
so `n+1` points will always uniquely determine a polynomial of degree `n`.

> interpolateN :: (Fractional a, Ord a) => [Point a] -> Polynomial a
> interpolateN ps =
>   poly $ \x -> sum $ zipWith (*) (fmap val coeffs) [ x^i | i <- [numPoints - 1, numPoints - 2 .. 0]]
>   where
>     numPoints = length ps
>     m = (numPoints, numPoints) # [ x^i | x <- fmap fst ps, i <- [numPoints - 1, numPoints - 2 .. 0]]
>     Column coeffs = solve m (Column $ fmap snd ps)
>
> interpolateNPlots :: [Point Double] -> [Plot]
> interpolateNPlots ps = [polyPlot $ interpolateN ps, points ps]

Said in the language of linear algebra:

The system of equations

```
c_0 + c_1*x_0 + ... + c_{n-1}*x_0^{n-1} + c_n*x_0^n = y_0
c_0 + c_1*x_1 + ... + c_{n-1}*x_1^{n-1} + c_n*x_1^n = y_1
... + ...     + ... + ...               + ...       = ...
c_0 + c_1*x_n + ... + c_{n-1}*x_n^{n-1} + c_n*x_n^n = y_n
```

always has a solution, and the matrix

```
1  x_0  ...  x_0^{n-1}  x_0^n
1  x_1  ...  x_1^{n-1}  x_1^n
...     ...  ...        ...
1  x_n  ...  x_n^{n-1}  x_n^n
```

(a [Vandermonde matrix](https://en.wikipedia.org/wiki/Vandermonde_matrix)) always has an
inverse.

I think it's important to avoid getting too caught up in matrices when working with linear
algebra. Instead of saying "the solution to `Ax = b` is a vector of coefficients of a
polynomial", I'd rather say "the solution to `Ax = b` *is* a polynomial".
Concretely, though, `x` is actually a vector. So why am I justified in calling it a
polynomial?

Because I've intuitively chosen a basis for the vector space of n-degree polynomials, along
with a vector space homomorphism (a.k.a a linear map) from vectors of length `n` to
these polynomials:

* Basis: `{λx. x^0, λx. x^1, ..., λx. x^{n-1}, λx. x^n}` ([monomial basis](https://en.wikipedia.org/wiki/Monomial_basis))
* Linear map: `P = (λx. x^0, λx. x^1, ..., λx. x^{n-1}, λx. x^n)`

Writing the system of equations

```
c_0 + c_1*x_0 + ... + c_{n-1}*x_0^{n-1} + c_n*x_0^n = y_0
c_0 + c_1*x_1 + ... + c_{n-1}*x_1^{n-1} + c_n*x_1^n = y_1
... + ...     + ... + ...               + ...       = ...
c_0 + c_1*x_n + ... + c_{n-1}*x_n^{n-1} + c_n*x_n^n = y_n
```

as a matrix-vector product of matrix a Vandermonde matrix

```
1  x_0  ...  x_0^{n-1}  x_0^n
1  x_1  ...  x_1^{n-1}  x_1^n
...     ...  ...        ...
1  x_n  ...  x_n^{n-1}  x_n^n
```

with (variable) vector

```
c_0
c_1
...
c_{n-1}
c_n	
```

all equal to results vector

```
y_0
y_1
...
y_{n-1}
y_n	
```

implies this particular vector-to-polynomial map.

What polynomial is represented by the right-hand side vector of `Ax = b`? Is that even a
reasonable question? I'm not sure. But here's what I do know:

When `A` is invertible, we have `x = (A^-1)b`. Since `x` represents a polynomial via `P`,
`(A^-1)b` does too. `x` is a linear combination of the columns of `A^-1`, so each of those
columns also represent a polynomial in the same way. Therefore the vector `b` represents
the same polynomial as `x` but uses the basis given by `P(A^-1)`.

We can find the columns of `A^-1` by inverting `A`, but is there a simpler way?

The first thing I notice is that the inverse of `A` is independent of `b`.
`P((A^-1)b)` interpolates any dataset having the `x` coordinates that were "baked in" to
`A^-1`. In other words, `P((A^-1)b)(x_i) = y_i` for every `b = [y_0; y_1; ...; y_n]`.
If `A^-1` consists of the columns `p_0`, `p_1`, ..., it follows that
`[P(p_0)(x_i) P(p_1)(x_i) ... P(p_n)(x_i)]b = y_i` for all `b`.

Every `b` is a linear
combination of the vectors `e_1 = [1; 0; ...; 0;]`, `e_2 = [0; 1; ...; 0]`, ..., `e_n = [0; 0; ...; 1]`
(the [standard basis](https://en.wikipedia.org/wiki/Standard_basis)).
So taking `b = e_1` for example, we must have

```
[P(p_0)(x_0) P(p_1)(x_0) ... P(p_n)(x_0)]e_1        = 1
[P(p_0)(x_1) P(p_1)(x_i) ... P(p_n)(x_1)]e_1        = 0
...                                                 = 0
[P(p_0)(x_n) P(p_1)(x_i) ... P(p_n)(x_n)]e_1        = 0

meaning

```
1*P(p_0)(x_i) + 0*P(p_1)(x_i) + ... + 0*P(p_n)(x_i) = 1 when i = 0, or 0 otherwise
P(p_0)(x_i)                                         = 1 when i = 0, or 0 otherwise
```

similarly, for `b = e_2` we have

```
[P(p_0)(x_i) P(p_1)(x_i) ... P(p_n)(x_i)]e_2        = 1 when i = 1, or 0 otherwise
0*P(p_0)(x_i) + 1*P(p_1)(x_i) + ... + 0*P(p_n)(x_i) = 1 when i = 1, or 0 otherwise
P(p_1)(x_i)                                         = 1 when i = 1, or 0 otherwise
```

and so on.

So the `i`th column of `A^-1` represents a polynomial that has `f(x_i) = 1` and `f(x_j) = 0` for `j != i`.

For `i = 0`, the polynomial `f(x) = (x - x_1)*...*(x - x_n)` is 0 in all the right places.
`f(x_0) = (x_0 - x_1)*...*(x_0 - x_n)`, so the function `l_0(x) = (x - x_1)*...*(x - _n) / (x_0 - x_1)*...*(x_0 - x_n)`
has `l_0(x_0) = 1` and `l_0(x_j) = 0` for `j != 0`.
This is the first [Lagrange polynomial](https://en.wikipedia.org/wiki/Lagrange_polynomial)
for nodes `x_0, x_i, ..., x_n`.

> lagrange :: Fractional a => [a] -> Int -> Polynomial a
> lagrange nodes i
>   | i < length nodes =
>     let ([(_, target)], rest) = partition ((== i) . fst) (zip [0..] nodes) in
>     poly $ \x ->
>       product (fmap (\(_, x') -> x - val x') rest) *
>       val (1 / product (fmap (\(_, x') -> target - x') rest))
>   | otherwise = error $ "index >= number of nodes (" ++ show i ++ " >= " ++ show (length nodes) ++ ")"

The Lagrange polynomials for `n+1` nodes are apparently a basis for the vector space of
n-degree polynomials. This isn't intuitively obvious to me, so I wonder if I can prove it.

I can tell that no n-degree polynomials are ruled out by a particular choice of `n+1` nodes.
We're dealing with functions from reals to reals, so every choice of `f(x_0)`, `f(x_1)`, ..., `f(x_n)` for nodes `x_0, x_1, ..., x_n` belongs to a valid polynomial. This demonstrates
that it's *possible* for the Lagrange polynomials of `x_0, x_1, ..., x_n` to span the
n-degree polynomials, but it doesn't prove that they actually do.

Let's take an arbitrary linear combination of the Lagrange polynomials for a set of nodes:

```
f(x) = y_0*l_0(x) + y_1*l_1(x) + ... + y_n*l_n(x)
```

I suppose that spanning would be proved if I can show that every polynomial of the form

```
f(x) = c_0*x^n + c_1*x^{n-1} + ... + c_{n-1}*x + c_0
```

can be rewritten as a linear combination of the Lagrange polynomials for `n+1` nodes.

> lagrangeBasis :: Fractional a => [a] -> [Polynomial a]
> lagrangeBasis nodes = lagrange nodes <$> [0..length nodes - 1]
>
> lagrangePlots :: [Double] -> [Plot]
> lagrangePlots nodes =
>   points (fmap (\x -> (x, 1)) nodes) :
>   fmap polyPlot (lagrangeBasis nodes)
>
> inverseVandermonde :: Fractional a => [a] -> Matrix a
> inverseVandermonde nodes = 
>   Matrix
>     numNodes
>     numNodes
>     (fmap (\(Polynomial cs) -> Column cs) (lagrangeBasis nodes))
>   where
>     numNodes = length nodes
>
> lagrangeInterpolateN :: Fractional a => [Point a] -> Polynomial a
> lagrangeInterpolateN ps =
>   let Column p = (inverseVandermonde (fmap fst ps) `compose` ((numPoints, 1) # fmap snd ps))!0 in
>   Polynomial p
>   where
>     numPoints = length ps
>
> lagrangeInterpolateNPlots :: [Point Double] -> [Plot]
> lagrangeInterpolateNPlots ps = [points ps, polyPlot $ lagrangeInterpolateN ps]
