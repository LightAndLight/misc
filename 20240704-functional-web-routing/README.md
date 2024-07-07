# `functional-web-routing`

*2024-07-09*

This started out as an attempt to do trie-based URL routing on well-typed web route
specifications. For example, the URL `posts/{postId}/edit` could be specified as
something like `"posts" // param "postId" @Int // "edit"`, having a type
like `Path (Int -> b)`. Each such path can be used as a key in a special trie that
a web server uses to do routing. One consequence of typed URLs is well-typed links:
if a `Path` has a parameter, then converting that `Path` to a URL should take an extra
argument for the parameter value. For example, given some `toUrl` function,
`toUrl ("posts" // param "postId" @Int // "edit")` should have type `Int -> String`,
with `toUrl ("posts" // param "postId" @Int // "edit") 42` giving `"posts/42/edit"`.

I ended up with a [servant](https://hackage.haskell.org/package/servant)-like web
framework, but with a much smaller type-level DSL. If I polish it then I'll copy
the code into a new repo / Haskell package and link to it from here.
