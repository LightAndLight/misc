{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (fold)
import Data.Time.Clock (getCurrentTime)
import Lib

app :: App
app =
    fold
        [ page
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
                perform (sample eButtonClicked bInputVal) (\((), value) -> putStrLn $ "The value is " <> value)

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
            ("example" <> "recieve")
            ( do
                buttonEl <- element $ Node "button" [] [Text "Click me!"]
                let eButtonClicked = domEvent Click buttonEl

                eCurrentTime <- request eButtonClicked (\() -> show <$> getCurrentTime)
                let rCurrentTime = stepper "The button hasn't been clicked yet." eCurrentTime

                pure
                    $ Html
                        [ Node "head" [] [Node "title" [] [Text "Example - click and recieve"]]
                        , Node
                            "body"
                            []
                            [ Node "p" [] [Text "When you click the button, the server will respond with the current time."]
                            , ReactiveText rCurrentTime
                            , html buttonEl
                            ]
                        ]
            )
        ]

main :: IO ()
main = serve app
