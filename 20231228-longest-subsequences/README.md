# Longest subsequences

Partitioning a list into its longest sequential subsequences.

The specification is pretty nice:

```haskell
consecutiveRanges :: (Ord a, Enum a) => [a] -> [[a]]
consecutiveRanges =
  unfoldr
    ( \s ->
        if null s
          then Nothing
          else Just $ maximumBy (compare `on` length) . filter (consecutive . fst) $ zip (inits s) (tails s)
    )
 where
  consecutive [] = True
  consecutive xs@(_ : ys) = all (\(a, b) -> succ a >= b) (zip xs ys)
```

Problem is posed in <https://lobste.rs/s/dx67kd/fontimize_subset_fonts_exactly_only_your#c_ydwydk>.
