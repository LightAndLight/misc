# Simple text search and ranking

*2025-01-08*

While using [Spacemacs](https://www.spacemacs.org/) I noticed that it returned fuzzy-matching
results in an order I didn't like:

Query: `inline`

Results:

1. ```
   When to inline in Haskell?
           ^^^^^^
   ```

1. ```
   POSIX command line syntax
      ^       ^  ^^^^
   ```

1. ```
   Meaningful choice in games
       ^^   ^    ^    ^    ^
   ```

1. ```
   Machine learning resources
       ^^  ^    ^^   ^
   ```

1. ```
   Blizzard's technical lineage
     ^            ^   ^  ^^^
   ```

1. ```
   Designing an inlining heuristic
      ^ ^         ^^^     ^
   ```

1. ```
   Deriving a CLI parser interface
      ^  ^     ^^         ^ ^
   ```

I see what it's doing; it finds the first occurrence of each query string character while scanning
over the target string. The limitation of this method is obvious in the `Designing an inlining heuristic`
target. I would want to see that entry at position two, because it has a very strong substring match:

```
Designing an inlining heuristic
             ^^^^^
```

I'd like to design a little search algorithm that finds the longest prefix of the query string,
and see how that performs.
