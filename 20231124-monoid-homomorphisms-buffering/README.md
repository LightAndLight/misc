# Monoid homomorphisms and buffered processing

*2023-11-24*

I was wondering what sort of functions "behaved well" when run over buffered input.

For example, 

```haskell
isHelloWorld :: ByteString -> Bool
isHelloWorld "hello world" = True
isHelloWorld _ = False
```

needs to see at least `length "hello world" == 11` bytes before it can return a result, so if I could only afford to
allocate a `ByteString` of length 5 then I'd never be able to run this function.

I'd have to rewrite the function in an incremental style so it can make progress with whatever size
input it receives:

```haskell
newtype Matcher a = Matcher { feed :: ByteString -> Either (Matcher a) a }

isHelloWorld :: Matcher Bool
isHelloWorld = go "hello world"
  where
    go :: ByteString -> Matcher Bool
    go s =
      Matcher $ \input ->
        case ByteString.stripPrefix input s of
          Nothing ->
            Right False
          Just s' ->
            if ByteString.null s'
              then Right True
              else Left (go s')
```

A well-behaved function with respect to buffering is one that works for any positive buffer size.
Mapping, folding, and filtering fit; you can do these operations over an entire dataset, or you can
do it a chunk at a time while recombining the results.

I noticed that all the operations I thought of are monoid homomorphisms:

```
map : (a -> b) -> List a -> List b
map f [] = []
map f (a <> b) = map f a <> map f b

filter : (a -> Bool) -> List a -> List a
filter f [] = []
filter f (a <> b) = filter f a <> filter f b

foldl : (b -> a -> b) -> b -> List a -> b
flip (foldl f) [] = id
flip (foldl f) (a <> b) = flip (foldl f) a >>> flip (foldl f) b
```

The reason they fit my intuition of "well-behaved" is because they can be uniquely "lifted" over
chunks of any size.

Given `chunksOf : Nat -> m -> List m`, which splits a value into sequential, (satisfies
`forall n. fold . chunksOf n = id`) evenly-sized pieces, a monoid homomorphism `f : m -> m'` has the
following property:

```
f = foldMap f . chunksOf n
```

I think `foldMap f . chunksOf n : m -> m'` is a specification for processing a value using a fixed
buffer size of `n`.

If this is the case, I might be able to reframe "streaming" (processing large amounts of data using
constant memory) in terms of monoid homomorphisms.
This library is an experiment in programming with monoid homomorphisms and using them for efficient streaming.