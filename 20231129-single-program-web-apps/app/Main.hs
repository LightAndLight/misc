{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Compiler.Plugin.Interface (toString)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (fold)
import Data.Functor (void)
import Data.IORef
import Data.Time.Clock (getCurrentTime)
import SPWA.App (App, page, pageM, serve)
import SPWA.DomEvent (DomEvent (..))
import SPWA.Element (html)
import SPWA.Event (domEvent, sample)
import SPWA.Html (Html (..))
import SPWA.Interact (element, mkTrigger, onLoad, perform, request, stepperR, stepperRM, textInput)
import SPWA.Path (href)
import SPWA.Reactive (current)
import SPWA.Session (forkSession)
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
                    , Node "li" [] [Node "a" [href ("example" <> "onload")] [Text "Example 6 - \"onload\""]]
                    , Node "li" [] [Node "a" [href ("example" <> "trigger")] [Text "Example 7 - \"trigger\""]]
                    , Node "li" [] [Node "a" [href ("example" <> "dynamic")] [Text "Example 8 - \"dynamic\""]]
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
            rCurrentTime <- stepperRM (show <$> getCurrentTime) eCurrentTime

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
            rX <- stepperR "Unknown" $ fmap toString eX

            let eXPlusOne = fmap (toString . (+ 1)) eX
            rXPlusOne <- stepperR "Unknown" eXPlusOne

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

            rec rCount <- stepperR (0 :: Int) $ (\((), x) -> x + 1) <$> sample eButtonClicked (current rCount)

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
        ("example" <> "onload")
        ( do
            onLoad . liftIO $ putStrLn . ("page loaded at " <>) . show =<< getCurrentTime

            pure
              $ Html
                [ Node "head" [] [Node "title" [] [Text "Example - onload"]]
                , Node
                    "body"
                    []
                    [ Node
                        "p"
                        []
                        [Text "When the page loads, the server will run an action."]
                    ]
                ]
        )
    , pageM
        ("example" <> "trigger")
        ( do
            (sendCount, eCount) <- mkTrigger @Int

            onLoad . void . forkSession $ do
              countRef <- liftIO $ newIORef 0
              forever $ do
                count <- liftIO $ readIORef countRef
                liftIO $ writeIORef countRef $ count + 1
                sendCount count
                liftIO $ threadDelay 1000000

            rec rCount <- stepperR "no count recieved" $ toString <$> eCount

            pure
              $ Html
                [ Node "head" [] [Node "title" [] [Text "Example - trigger"]]
                , Node
                    "body"
                    []
                    [ Node
                        "p"
                        []
                        [Text "When the page loads, the server will start counting, sending the counter to the client, and the client renders the counter."]
                    , Node "p" [] [ReactiveText rCount]
                    ]
                ]
        )
    , pageM
        ("example" <> "dynamic")
        ( do
            let
              text True = "This is some bold text"
              text False = "This is some italic text"

            buttonEl <- element $ Node "button" [] [Text "toggle"]
            let eButtonClicked = domEvent Click buttonEl

            rec rBold <- stepper True $ not . snd <$> sample eButtonClicked (current rBold)
            eHtml <-
              request
                (sample eButtonClicked (current rBold))
                ( \((), bold) ->
                    let next = not bold
                     in pure $ Node (if next then "b" else "i") [] [Text $ text next]
                )
            rHtml <- stepper (Node "b" [] [Text $ text True]) eHtml

            pure
              $ Html
                [ Node "head" [] [Node "title" [] [Text "Example - dynamic"]]
                , Node
                    "body"
                    []
                    [ Node
                        "p"
                        []
                        [ Text "Clicking the button will toggle between bold and italic."
                        , Text " "
                        , Text "The new HTML generated on the server, sent to the client, and then inserted into the page."
                        ]
                    , Node "p" [] [ReactiveHtml rHtml]
                    , html buttonEl
                    ]
                ]
        )
    ]

main :: IO ()
main = serve 8000 app
