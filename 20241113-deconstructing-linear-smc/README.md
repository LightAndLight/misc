# Deconstructing `linear-smc`

*2024-11-13*

My friend [Jack Kelly](http://jackkelly.name/blog/) told me about [`linear-smc`](https://hackage.haskell.org/package/linear-smc-2.0.2/docs/Control-Category-Linear.html),
a Haskell library for defining [symmetric monoidal category](https://ncatlab.org/nlab/show/symmetric+monoidal+category) arrows using (linear) Haskell functions.
It's similar to [`concat`](https://github.com/compiling-to-categories/concat) in that you can think of it as using Haskell syntax for a categorical construction.
The advantage of `linear-smc` over `concat` is that it can be defined as a library instead of a compiler plugin. I want to understand how it does this.
