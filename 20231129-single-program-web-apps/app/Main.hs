{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

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
            ("example" <> "receive")
            ( do
                buttonEl <- element $ Node "button" [] [Text "Click me!"]
                let eButtonClicked = domEvent Click buttonEl

                eCurrentTime <- request eButtonClicked (\() -> show <$> getCurrentTime)
                rCurrentTime <- stepperM (show <$> getCurrentTime) eCurrentTime

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

                eZero <- request eButtonClicked (\() -> pure (0 :: Int))
                let eZeroPlusOne = fmap (toString . (+ 1)) eZero
                rCurrentTime <- stepper "" eZeroPlusOne

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
        ]

main :: IO ()
main = serve app
