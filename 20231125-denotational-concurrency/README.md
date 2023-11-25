# Denotational concurrency

*2023-11-25*

Recently I've getting into denotational design, and I starting wondering how to model concurrency.
Single-threaded state can be represented as a function `s -> (s, a)` (the State monad), but what
about shared state? How do I talk about state that may change due to external factors in between my
interactions with it?

When looking into the semantics of concurrently, nondeterminism came up as a simple candidate. For
example, Moggi (1991)[^1] suggests using the power set monad for nondeterminism. In Haskell, we
model nondeterminism usings lists, so I've tried to use [the list monad
transformer](https://hackage.haskell.org/package/list-transformer) in a way that represents
nondeterminism in the context of other effects.

I started with an interface for concurrency:

```haskell
class Monad m => MonadConc m where
  concurrently :: m a -> m b -> m (a, b)
```

My intuition says that `concurrently` means the evaluation of `m a` and `m b` should be arbitrarily
interleaved. I tried to find a monadic type that expresses this.

I've ended up with the following core:

```haskell
newtype ConcT t m a = ConcT {getConcT :: ComposeT t ListT m a}

instance (MApplicative t, Monad m) => MonadConc (ConcT t m) where
  concurrently (ConcT ma) (ConcT mb) =
    ConcT
      . ComposeT
      $ mapply
        (<|>)
        ( getComposeT $ do
            a <- ma
            b <- mb
            pure (a, b)
        )
        ( getComposeT $ do
            b <- mb
            a <- ma
            pure (a, b)
        )
```

`ConcT` is a monad transformer transformer [sic] that adds nondeterminism to the "bottom" of a monad
transformer stack. It has a definition of `concurrently` that satisfies my intuition for the meaning
of concurrency. Here are some things it can do:

* ```haskell
  do
    let results = runIdentity . collectListT . runExceptT . flip runStateT (0 :: Int) . getComposeT . runConcT $ do
          (_, s) <- concurrently (liftT $ modify (+ 2)) (liftT get)
          unless (s < 2) . liftT $ throwError "failure"
    print results
  ```

  Prints `[Left "failure",Right ((),2)]`.
  
  In `ConcT (ComposeT (StateT Int) (ExceptT String)) Identity ()`, if I read the state while
  concurrently modifying it, then there's one interleaving where I got `0`, found `0 < 2`, and
  succeeded. There's another intereaving where I got `2`, found `not (2 < 2)`, and threw an error.

* ```haskell
  do
    let results = runIdentity . collectListT . runWriterT . runConcT $ do
          _ <-
            liftT (tell "a")
              `concurrently` liftT (tell "b")
              `concurrently` liftT (tell "c")
          pure ()
    print results
  ```

  Prints `[((),"abc"),((),"bac"),((),"cab"),((),"cba")]`.

  In `ConcT (WriterT String) Identity ()`, it finds 4 interleavings of the three concurrent `tell`s.
  It's missing two: "acb" and "bca", because `concurrently` doesn't know that
  `tell "b"` and `tell "c"` are concurrent so it can't generate interleavings by swapping them. I
  don't know how to fix this at the moment.

* ```haskell
  do
    let results = runIdentity . collectListT . flip execStateT (0 :: Int, 1 :: Int) . runConcT $ do
          _ <- 
            concurrently 
              (liftT . modify $ first (+ 2))
              (liftT . modify $ second (* 3))
          pure ()
    print results
  ```

  Prints `[(2,3),(2,3)]`.

  In `ConcT (StateT (Int, Int)) Identity ()`, it shows that concurrently modifying different
  parts the state gives the same result. The operations commute!

Returning to the original question of shared state, maybe this is a start:

```
[[ State s a ]] = s -> (s, a)
[[ SharedState s a ]] = s -> [(s, a)]

get :: s -> SharedState s s
[[ get ]] = \inState -> (inState, inState) : [ (state, state) | state \in s ]

put :: s -> SharedState s ()
[[ put state ]] = \_ -> [(state, ())]

modify :: (s -> s) -> SharedState s ()
[[ modify f ]] = \inState -> (f inState, ()) : [ (f state, ()) | state \in s ]

atomically :: State s a -> SharedState s a
[[ atomically m ]] = \inState -> [ [[ m ]] s ]
```

[^1]: Moggi, E. (1991). Notions of computation and monads. Information and computation, 93(1),
    55-92.