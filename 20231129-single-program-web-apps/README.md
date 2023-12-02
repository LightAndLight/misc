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

It all works well, except my FRP implementation has mistakes that I don't know how to fix.
I think that's orthogonal to the validity of the project;
the proof-of-concept still holds.

Inspirations:

* Shen, G., Kashiwa, S., & Kuper, L. (2023). Haschor: Functional choreographic programming for
    all (functional pearl). arXiv preprint arXiv:2303.00924.

* [Haste Haskell](https://web.archive.org/web/20221004075724/https://www.haste-lang.org/)