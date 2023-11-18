# GHC GADTs bug

*2023-11-18*

```
$ nix develop

$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.6.3
```

`getIndex Nil Z` and `getIndex Nil S{}` are recognised as inaccessible pattern matches, but when
they are omitted GHC reports an incomplete pattern match.

```
$ cabal build
...

src/Lib.hs:21:1: warning: [GHC-94210] [-Woverlapping-patterns]
    Pattern match has inaccessible right hand side
    In an equation for ‘getIndex’: getIndex Nil Z = ...
   |
21 | getIndex Nil Z = undefined
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^

src/Lib.hs:21:14: warning: [GHC-40564] [-Winaccessible-code]
    • Inaccessible code in
        a pattern with constructor:
          Z :: forall a (as :: [*]). Index (a : as) a,
        in an equation for ‘getIndex’
      Couldn't match type: '[]
                     with: a : as
    • In the pattern: Z
      In an equation for ‘getIndex’: getIndex Nil Z = undefined
    • Relevant bindings include
        getIndex :: Context f ctx -> Index ctx a -> f a
          (bound at src/Lib.hs:19:1)
   |
21 | getIndex Nil Z = undefined
   |              ^

src/Lib.hs:22:1: warning: [GHC-53633] [-Woverlapping-patterns]
    Pattern match is redundant
    In an equation for ‘getIndex’: getIndex Nil S {} = ...
   |
22 | getIndex Nil S{} = undefined
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

src/Lib.hs:22:14: warning: [GHC-40564] [-Winaccessible-code]
    • Inaccessible code in
        a pattern with constructor:
          S :: forall (as :: [*]) a b. Index as a -> Index (b : as) a,
        in an equation for ‘getIndex’
      Couldn't match type: '[]
                     with: b : as
    • In the pattern: S {}
      In an equation for ‘getIndex’: getIndex Nil S {} = undefined
   |
22 | getIndex Nil S{} = undefined
   |

$ patch src/Lib.hs src/comment_out.patch

$ cabal build

...

src/Lib.hs:19:1: warning: [GHC-62161] [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘getIndex’:
        Patterns of type ‘Context f ctx’, ‘Index ctx a’ not matched: Nil _
   |
19 | getIndex (Cons a _) Z = a
   | ^^^^^^^^^^^^^^^^^^^^^^^^^...
```