# Single-program web apps

*2023-11-29*

Web apps usually have separate "frontend" and "backend" codebases, where the backend implements a
HTTP server and the frontend implements a HTML + JavaScript UI + client-to-the-backend-server.
Oftentimes these JavaScript UIs reimplement a bunch of web browser features, such at keeping the
browser URL up-to-date when links are clicked.

What if that distinction and all the associated work is *accidental* complexity?

In this project I experiment with programming as if a web app (comprised of client-side HTML + JS
and a remote HTTP server) is actually a *single* program.

My first example is writing Haskell code for a HTML page as if the HTML page could call native
haskell functions:

```haskell
Html
  [ Node "head" [Node "title" [Text "Test path"]]
  , Node
      "body"
      [ Node "p" [Text "When you click the button, an IO action is run on the server."]
      , Node "button" [Text "Click me!"] `OnEvent`
          (Click, putStrLn "The button was clicked!")
      ]
  ]
```

The above code represents a HTML page containing a button that, when clicked, calls a Haskell function
of type `IO ()`.

The second example is a page like the first, but it can pass an argument to the Haskell
function:

```haskell
do
  (bInputVal, inputEl) <- textInput

  buttonEl <- element $ Node "button" [] [Text "Click me!"]
  let eButtonClicked = domEvent Click buttonEl
  perform
    (sample eButtonClicked bInputVal)
    (\((), value) -> putStrLn $ "The value is " <> value)

  pure $
    Html
      [ Node "head" [] [Node "title" [] [Text "Example - click and send"]]
      , Node
          "body"
          []
          [ Node "p" [] [Text "When you click the button, an IO action is run on the server using the contents of the text input."]
          , html inputEl
          , html buttonEl
          ]
      ]
```

When the button is clicked, the contents of the text field are passed to the Haskell function.
This requires interactively sampling client-side state (the content of the text field), which is
expressed with FRP.

I also wrote a GHC plugin that I use to translate simple Haskell expressions to JavaScript,
for non-trivial FRP code that runs on the client, e.g.

```haskell
rec rCount <- stepperR (0 :: Int) $ (\((), prev) -> prev + 1) <$> sample eButtonClicked (current rCount)
``` 

Which keeps a client-side counter that's incremented via JavaScript on a button click.

## Wishlist

* Event triggers

  `mkTrigger :: Send a => Interact (a -> IO (), Event a)`
  
  `mkTrigger` creates a callback that can be used by the server to trigger an event on the client.
  Pages that use `mkTrigger` open a websocket connection that recieves the data passed to the server's callback.
  The `Event a` is fired for each client-side `message` event.

* Dynamic HTML

  At the moment I have dynamic text nodes, through `ReactiveText :: Reactive Text -> Html`.
  I want to generalise this to arbitrary HTML with a `ReactiveHtml :: Reactive Html -> Html`.
  
  That's a lot more complicated for two reasons:
  
  1. The `Send` constraint on `stepper :: Send a => a -> Event a -> Interact (Reactive a)`
  
     To create a `Reactive Html` using `stepper`, I need `instance Send Html`.
     A `Send a` instance means there's an isomorphism between an `a` and its JS representation.
     That way `stepper initial eUpdate` can compile `initial` to JS and embed it in the page / script.
     `instance Send Html` is easy to write for the `Node` constructor, but how do I encode
     `ReactiveText :: Reactive Text -> Html` and `ReactiveHtml :: Reactive Html -> Html`?
     I'm uncertain how this works out.

  2. Haskell to JS code generation for `Event` and `Reactive` combinators, such as
     `fmap :: (a -> b) -> Event a -> Event b` and `fmap :: (a -> b) -> Reactive a -> Reactive b`.
    
     To get a `Reactive Html` I need a function `a -> Html` because there are no `Event Html` primitives.
     I could build the `Reactive Html` directly using `stepper` taking an `Event Html`,
     which must at some point involve an `fmap (f :: a -> Html) (event :: Event a) :: Event Html`.
     Or I could build a `Reactive a` and then `fmap` over it to get
     `fmap (f :: a -> Html) (reactive :: Reactive a) :: Reactive Html`.
     Both of these paths require a JS representation of an `a -> Html`, but
     making my "quote" compiler plugin work for `Html` values is an ordeal.
     
     Ideally the plugin would work properly for arbitrary datatypes, through some kind of
     sums-of-products decomposition.
     I thought about adding an overloaded function `quote :: Quote a => a -> Expr '[] (QuoteTy a)`
     and having the compiler plugin insert calls to it when it encounters constructors for
     user-defined datatypes.
     The plugin operates over core, so I expect it will be difficult to resolve the type class
     dictionaries when we want to insert `quote` calls.
     
     Maybe I can push the plugin into the type checker / constraint solver.
     Calls to `quote` that actually have instances can be skipped.
     Calls matching `quote @(a -> b) f` can synthesise a dictionary based on the simplified
     core of the term `f`.
     This would change the meaning of `quote`; `quote (f x) == App (quote f) (quote x)` would no longer be true in general.
     Maybe you could call this "extensional quoting" as opposed to "intensional quoting". It seems worse.
     It also might not work properly because I need to inline terms containing `quote` until its
     argument has no free variables.
    
     Alternatives:
    
     * Just construct `Expr`s - i.e. `fmapEvent :: Expr '[] (a -> b) -> Event a -> Event b`
       * Make it as easy as possible: PHOAS / quasiquoter
       * Downside: no reuse of existing Haskell code
     
     * Run the app source code through a Haskell to JS compiler (modified GHCJS / GHC with JS backend) to "project out"
       the JS portion of the app
      
       Mental model: compiling my app with GHC creates a server executable, compiling my app
       with GHC+JSbackend+plugin creates client JS code required by each page of the server.

## Inspirations / similar projects

* Shen, G., Kashiwa, S., & Kuper, L. (2023). Haschor: Functional choreographic programming for
    all (functional pearl). arXiv preprint arXiv:2303.00924.

* [Haste Haskell](https://web.archive.org/web/20221004075724/https://www.haste-lang.org/)

* [Ocaml Ocsigen](https://ocsigen.org)