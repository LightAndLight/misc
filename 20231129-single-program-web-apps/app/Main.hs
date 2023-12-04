{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Data.Foldable (fold)
import Data.Functor ((<&>))
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import Lib
import System.Random (randomRIO)

app :: App
app =
  fold
    [ page
        mempty
        ( Html
            [ Node "head" [] [Node "title" [] [Text "Examples"]]
            , Node
                "body"
                []
                [ Node "p" [] [Text "Index of examples"]
                , Node
                    "ul"
                    []
                    [ Node "li" [] [Node "a" [href ("example" <> "click")] [Text "Example 1 - \"click\""]]
                    , Node "li" [] [Node "a" [href ("example" <> "send")] [Text "Example 2 - \"send\""]]
                    , Node "li" [] [Node "a" [href ("example" <> "receive")] [Text "Example 3 - \"receive\""]]
                    , Node "li" [] [Node "a" [href ("example" <> "map")] [Text "Example 4 - \"map\""]]
                    , Node "li" [] [Node "a" [href ("example" <> "counter")] [Text "Example 5 - \"counter\""]]
                    , Node "li" [] [Node "a" [href ("example" <> "todo")] [Text "Example 6 - \"todo\""]]
                    ]
                ]
            ]
        )
    , page
        ("example" <> "click")
        ( Html
            [ Node "head" [] [Node "title" [] [Text "Example - click"]]
            , Node
                "body"
                []
                [ Node "p" [] [Text "When you click the button, an IO action is run on the server."]
                , Node "button" [] [Text "Click me!"] `OnEvent` (Click, putStrLn "The button was clicked!")
                ]
            ]
        )
    , pageM
        ("example" <> "send")
        ( do
            (bInputVal, inputEl) <- textInput

            buttonEl <- element $ Node "button" [] [Text "Click me!"]
            let eButtonClicked = domEvent Click buttonEl
            perform (sample eButtonClicked bInputVal) (\((), value) -> putStrLn $ "The value is " <> Text.unpack value)

            pure
              $ Html
                [ Node "head" [] [Node "title" [] [Text "Example - click and send"]]
                , Node
                    "body"
                    []
                    [ Node "p" [] [Text "When you click the button, an IO action is run on the server using the contents of the text input."]
                    , html inputEl
                    , html buttonEl
                    ]
                ]
        )
    , pageM
        ("example" <> "receive")
        ( do
            buttonEl <- element $ Node "button" [] [Text "Click me!"]
            let eButtonClicked = domEvent Click buttonEl

            eCurrentTime <- request eButtonClicked (\() -> Text.pack . show <$> getCurrentTime)
            rCurrentTime <- stepperM (Text.pack . show <$> getCurrentTime) eCurrentTime

            pure
              $ Html
                [ Node "head" [] [Node "title" [] [Text "Example - click and recieve"]]
                , Node
                    "body"
                    []
                    [ Node "p" [] [Text "When you click the button, an updated time will be fetched from the server."]
                    , Text "Current time: "
                    , ReactiveText rCurrentTime
                    , html buttonEl
                    ]
                ]
        )
    , pageM
        ("example" <> "map")
        ( do
            buttonEl <- element $ Node "button" [] [Text "Click me!"]
            let eButtonClicked = domEvent Click buttonEl

            eX <- request eButtonClicked (\() -> randomRIO (0, 10) :: IO Int)
            rX <- stepper "Unknown" $ fmap toString eX

            let eXPlusOne = fmap (toString . (+ 1)) eX
            rXPlusOne <- stepper "Unknown" eXPlusOne

            pure
              $ Html
                [ Node "head" [] [Node "title" [] [Text "Example - click and recieve"]]
                , Node
                    "body"
                    []
                    [ Node
                        "p"
                        []
                        [ Text "When you click the button, the server will send a number "
                        , Node "i" [] [Text "x"]
                        , Text ", and the browser will display the result of "
                        , Node "i" [] [Text "x + 1"]
                        ]
                    , Node
                        "p"
                        []
                        [ Text "x: "
                        , ReactiveText rX
                        ]
                    , Node
                        "p"
                        []
                        [ Text "x + 1: "
                        , ReactiveText rXPlusOne
                        ]
                    , html buttonEl
                    ]
                ]
        )
    , pageM
        ("example" <> "counter")
        ( do
            buttonEl <- element $ Node "button" [] [Text "Click me!"]
            let eButtonClicked = domEvent Click buttonEl

            rec rCount <- stepper (0 :: Int) $ (\((), x) -> x + 1) <$> sample eButtonClicked (current rCount)

            pure
              $ Html
                [ Node "head" [] [Node "title" [] [Text "Example - click and recieve"]]
                , Node
                    "body"
                    []
                    [ Node
                        "p"
                        []
                        [ Text "When you click the button, the client will increment a count."
                        ]
                    , Node
                        "p"
                        []
                        [ Text "count: "
                        , ReactiveText $ fmap toString rCount
                        ]
                    , html buttonEl
                    ]
                ]
        )
    , pageM
        ("example" <> "todo")
        ( do
            (bInputValue, inputEl) <- textInput
            buttonEl <- element $ Node "button" [] [Text "Add"]
            let eButtonClicked = domEvent Click buttonEl
            let eInputValueOnClick = snd <$> sample eButtonClicked bInputValue
            rec rTodos <- stepper [] $ (\(new, todos) -> todos <> [new]) <$> sample eInputValueOnClick (current rTodos)
            pure
              $ Html
                [ Node "head" [] [Node "title" [] [Text "Example - todo"]]
                , Node
                    "body"
                    []
                    [ Node
                        "p"
                        []
                        [ Text "To-do list"
                        ]
                    , Node
                        "p"
                        []
                        [ html inputEl
                        , html buttonEl
                        ]
                    , Node
                        "p"
                        []
                        [ ReactiveHtml
                            $ rTodos
                            <&> \todos ->
                              if isEmpty todos
                                then Node "i" [] [Text "No items in list"]
                                else Node "ul" [] $ (\todo -> Node "li" [] [Node "p" [] [Text todo]]) <$> todos
                        ]
                    ]
                ]
        )
    ]

main :: IO ()
main = serve app
