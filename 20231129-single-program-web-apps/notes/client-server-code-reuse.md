# Client/server code reuse

Reusing code across the client and the server is an important goal of this project.
The mental model of a "single program web app" doesn't distinguish between client and server.
I shouldn't have to reimplement a function so that it can run on the client;
it breaks the mental model and creates busywork.
I shouldn't have to think about whether I'm "allowed" to use a function in a
context that will end up on the client,
if the type checker permits it, then the app should work as written[^1].

I wrote a [GHC core plugin](../compiler-plugin) in an attempt to get more code reuse.
The more Haskell code I can translate to JavaScript, the better.
The plugin replaces occurrences of `quote a` with a (well-typed) AST that represents `a`.
I use that AST to generate JavaScript that's equivalent to the Haskell code of `a`.
Unfortunately, my approach is pretty limited.

To compile an arbitrary expression, I need access to all the definitions involved.
At the level of a core plugin, not all this information is available.
I only have access to unfoldings that are exposed in interface files,
so I can't quote expressions that (transitively) depend on a definition that has no unfolding
in scope.
I learned this when I tried to quote `show @Int 999`.
The implementation of `show @Int` does have an unfolding, but somewhere down the line it depends
on a local "worker" function for a definition in another module, and the worker has no unfolding.

If I want code reuse for Haskell, I think I need access to the source code.

How would I do this ideally?

The first question is how to "stage" these programs.
My current approach is a Haskell program that contains a reified representation of some of its own
code, and when I run the program it uses that reified code to generate JavaScript.

One hypothetical approach is based on the *avoidance* of any program containing its reified code.
Start with a meta-program in language X, that compiles to a Haskell program containing Haskell-native
X terms (compiled X to Haskell), as well as reified X terms (expressed X AST in Haskell).
The Haskell program uses the embedded X AST to generate JavaScript.
If I invent X, I lose a more general form of code reuse: I have to build the X standard library.
All the Haskell functions in Hackage are now useless in X, unless I have a way to reflect Haskell into X.
If I do that, I still need access to Haskell source code, because I may need to reify the X-reflected
Haskell.

Another approach takes X as an intermediate representation, from which Haskell server code and JavaScript
client code is generated.
There's some Haskell-to-X compiler that compiles Haskell code to X and reifies relevant Haskell code into X,
and from X the server and client code is generated.
This is kind of similar to my current approach, where X is GHC Core, which is compiled into server code that
generates client code.

I end up circling back to "reusing arbitrary Haskell code" across the client and server requires full
(including transitive dependencies) Haskell source code access, a.k.a. a full-blown Haskell compiler.
I don't want to write my own Haskell compiler, so reusing GHC is my best option.

My first thought is to line things up so that GHC can compile a variant of my app into a server,
and GHC+JS can compile a different variant of my app into corresponding client JS.
I'm pessimistic about the quality of the code output by GHC+JS, because it includes a Haskell runtime.
Some sort of runtime seems necessary for semantics-preserving reuse (my compiler plugin changes strictness of programs)
of arbitrary Haskell code.
But I wouldn't be surprised if GHC+JS reimplements some JavaScript features in terms of other JavaScript
features because of the level at which the JavaScript is generated in the GHC pipeline.
I should actually play around with it to find out.
While it works for the server, it might not be what I want for the client code.
I need several JS files; one per page of the app.
My app, and its page definitions, might be written in a single file.
I'm not sure how to make a GHC+JS system split that up properly.

My second thought is an alternative codegen backend for GHC.
Maintaining a GHC fork is a burden and probably dooms one to obscurity and keeping up with code churn.

[^1]: The mental model doesn't take performance into account, because that's an implementation
    detail. It's reasonable to break out of the "single program" mental model, and think in
    terms of implementation details, when performance is a concern. For example, if an operation
    is quick and frequently peformed then it might be better to move to the client, and if an
    operation is requires a lot of computation and is run infrequently then it may be better to
    run on a server. There could be semantically identical functions `fmap :: (a -> b) -> Event a -> Event b`
    and `request :: (a -> b) -> Event a -> Event b` that perform a computation in different
    locations.