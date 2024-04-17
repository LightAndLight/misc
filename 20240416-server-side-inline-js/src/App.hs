{-# language TypeApplications #-}
{-# language GADTs #-}
{-# language QualifiedDo #-}
{-# language OverloadedLists #-}
{-# language DerivingStrategies #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module App (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)
import Data.Foldable (find)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Network.Wai.Handler.Warp
import Servant.Server (Application, Handler, serve, err404, ServerError (..), err400)
import Servant.Server.Generic (AsServer)
import qualified Html.Class as Html (Html(..))
import Js ((.=))
import qualified Js
import qualified Js.Promise as Promise
import Lib (Page(..), MultitierLinks, allMultitierLinks, AppendStr, UrlEncode, Client (..), unC, Server (..), FromRecord(..), ToRecord(..), HasCoerce, coerce, js)
import Data.IORef (readIORef, modifyIORef)
import Control.Monad (replicateM)
import System.Random (randomRIO)
import Html.Class (Html)
import Data.Functor ((<&>))
import GHC.Exts (fromList)
import Id (runFresh)
import qualified Data.Text as Text
import Db (PostSlug, CommentSlug(..), Post, Comment (..), CommentId (..))
import qualified Db
import App.Api (Routes (..), CreateComment (..), CreatedComment (..), api, decodeJsonCreatedComment)
import qualified Maybe

main :: IO ()
main = do
  let port = 8080
  putStrLn $ "Server running on http://localhost:" <> show port
  Network.Wai.Handler.Warp.run port app

app :: Application
app = serve api server

server :: Routes AsServer
server =
  Routes
  { home = home
  , viewPost = viewPost
  , createComment = createComment
  }

links :: (AppendStr l, UrlEncode l, IsString (l Text)) => Routes (MultitierLinks l)
links = allMultitierLinks

renderCommentId ::
  (IsString (l Text), Monoid (l Text), HasCoerce l) =>
  l CommentSlug ->
  l Text
renderCommentId slug = "comment-" <> coerce slug

home :: Handler Page
home = do
  posts <- liftIO $ readIORef Db.allPosts
  pure $ Page posts template
  where
    template :: [Post Server] -> Server (Html.Children Server)
    template posts =
      [ Html.el "head" [] [Html.el "title" [] [Html.text "Home"]]
      , Html.el "body" []
        [ Html.el "ul" [] . fromList $
          posts <&> \post ->
            Html.el "li" []
            [ Html.el "h2" []
              [ Html.el "a"
                  [Html.attr "href" $ links.viewPost post.slug]
                  [Html.text post.title]
              ]
            ]
        ]
      ]

viewPost :: PostSlug -> Handler Page
viewPost slug = do
  posts <- liftIO $ readIORef Db.allPosts
  case find (\post -> post.slug.unS == slug) posts of
    Nothing ->
      throwError err404
    Just post -> do
      comments <- liftIO $ readIORef Db.allComments
      let
        comments' =
          [ (S parentSlug, comment)
          | comment <- comments
          , comment.post == post.id
          , parentSlug <-
              case comment.parent.unS of
                Nothing ->
                  [Nothing]
                Just parentId -> 
                  [ Just parent.slug.unS
                  | parent <- comments
                  , parent.id == S parentId
                  ]
          ]
      pure $ Page (post, comments') template
  where  
    template ::
      (Post Server, [(Server (Maybe CommentSlug), Comment Server)]) ->
      Server (Html.Children Server)
    template (post, comments) = 
      [ Html.el "head" []
          [ Html.el "title" [] [Html.text post.title]
          , Html.el "style" []
              [ Html.text . S $
                Text.unlines
                  [ "[data-comment] > [data-comment] {"
                  , "  margin-left: 1em;"
                  , "}"
                  ]
              ]
          ]
      , Html.el "body" []
          [ Html.el "h1" [] [Html.text post.title]
          , Html.el "main" [] . fromList $
              comments <&> \(parentSlug, comment) ->
                renderComment post.slug parentSlug comment.slug comment.content
          , runFresh . js $ do
              Js.Func initComment <- fnInitComment
          
              commentNodes <- Js.const $ Js.document.querySelectorAll "[data-comment]"
              buttonDisplayVars <- Js.const $ Js.arrayOfLen 0

              Js.call commentNodes.forEach $
                Js.lam (\commentNode -> initComment (buttonDisplayVars, commentNode))
          ]
      ]
    
    renderComment ::
      ( HasCoerce l
      , Html l
      ) =>
      l PostSlug ->
      l (Maybe CommentSlug) ->
      l CommentSlug ->
      l Text ->
      l (Html.Node l)
    renderComment postSlug parentCommentSlug commentSlug content = do
      Html.el "div"
        [ Html.attr "id" $ renderCommentId commentSlug
        , Html.attr "data-comment" ""
        -- Is this wasteful? Maybe a single post slug should be implied for many comments.
        , Html.attr "data-post-slug" $ coerce postSlug
        , Html.maybeAttr "data-parent-comment-slug" $ Maybe.map coerce parentCommentSlug
        ]
        [ Html.el "p" [] [Html.text content]
        , Html.el "button" [Html.attr "data-comment-reply" ""] [Html.text "Reply"]
        ]

    fnInitComment ::
      Js.Statement
        r 
        (Js.Func (Js.Array Js.Text, Js.Node) ())
    fnInitComment = Js.func2 $ \(Js.Func initComment) (buttonDisplayVars, commentNode) -> do
      ix <- Js.const buttonDisplayVars.length
      buttonDisplayVars.length .= buttonDisplayVars.length + 1

      commentEl <- Js.refine @Js.Element commentNode

      replyButtonEl <- Js.unwrap $ commentEl.querySelector "[data-comment-reply]"
      
      Js.call
        replyButtonEl.addEventListener
        ("click", replyButtonOnClick initComment buttonDisplayVars ix commentEl replyButtonEl)
      where
        replyButtonOnClick initComment buttonDisplayVars ix commentEl replyButtonEl =
          Js.proc $ \_event -> do
            buttonDisplayVars.index ix .= replyButtonEl.style.display
            replyButtonEl.style.display .= "none"

            contentTextareaNode <-
              Js.const @Js.Node $
              Html.el "textarea" [Html.attr "name" "content"] []

            saveButtonNode <-
              Js.const @Js.Node $
              Html.el "button" [Html.attr "type" "submit"] [Html.text "Save"]

            cancelButtonNode <-
              Js.const @Js.Node $
              Html.el "button" [Html.attr "type" "button"] [Html.text "Cancel"]
    
            editorNode <-
              Js.const @Js.Node $
              Html.el "div" []
                [ contentTextareaNode
                , Html.el "div" []
                  [ saveButtonNode
                  , Js.cast cancelButtonNode
                  ]
                ]
            Js.expr $ commentEl.asNode.insertBefore (editorNode, replyButtonEl.asNode)

            Js.call
              cancelButtonNode.addEventListener
              ("click", cancelButtonOnClick buttonDisplayVars editorNode replyButtonEl ix)
    
            Js.call
              saveButtonNode.addEventListener
              ("click", saveButtonOnClick initComment buttonDisplayVars editorNode replyButtonEl ix commentEl contentTextareaNode)
        
        cancelButtonOnClick buttonDisplayVars editorNode replyButtonEl nodeIx =
          Js.proc $ \_event ->
            removeEditor buttonDisplayVars editorNode replyButtonEl nodeIx

        removeEditor buttonDisplayVars editorNode replyButtonEl nodeIx = do
          parentNode <- Js.unwrap editorNode.parentNode
          Js.expr $ parentNode.removeChild editorNode
          replyButtonEl.style.display .= buttonDisplayVars.index nodeIx

        saveButtonOnClick initComment buttonDisplayVars editorNode replyButtonEl ix commentEl contentTextareaNode =
          Js.proc $ \_event -> do
            commentHtmlEl <- Js.refine @Js.HTMLElement commentEl

            postSlug <- Js.as @PostSlug <$> Js.unwrap commentHtmlEl.dataset.postSlug
            parentCommentSlug <- do
              result <- Js.var_
              Js.match
                commentHtmlEl.dataset.parentCommentSlug
                (\case
                  Js.PUndefined ->
                    result .= Js.nothing
                  Js.PDefined a ->
                    result .= Js.just (Js.as @CommentSlug a)
                )
              pure result

            contentTextAreaEl <- Js.refine @Js.HTMLTextAreaElement contentTextareaNode
            content <- Js.const contentTextAreaEl.value

            options <- do
              options <- Js.var Js.newFetchOptions
              options.method .= Js.defined "POST"
              options.body .=
                Js.defined
                  (Js.stringify . 
                    unC .
                    fromRecord $
                    CreateComment{parent = C parentCommentSlug, content = C content}
                  )
              options.headers .= Js.defined (Js.object [("Content-Type", "application/json")])
              pure options

            Js.expr $ Promise.do
              response <-
                Js.window.fetchWithOptions
                  ( unC $ links.createComment (C postSlug)
                  , options
                  )
              value <- response.json ()
              Promise.lift_ $ do
                mCreatedComment <- decodeJsonCreatedComment value
                createdComment <- toRecord <$> Js.unwrap mCreatedComment
                
                -- I created a bug by using a Haskell `let` instead of `Js.const`.
                -- Can this be avoided by design?
                newCommentNode <-
                  Js.const $
                  renderComment postSlug parentCommentSlug createdComment.slug content
                Js.call initComment (buttonDisplayVars, newCommentNode)
                Js.call commentEl.append newCommentNode
                
                removeEditor buttonDisplayVars editorNode replyButtonEl ix

createComment :: PostSlug -> CreateComment Server -> Handler (CreatedComment Server)
createComment postSlug request = do
  mPost <- liftIO $ find ((postSlug ==) . (.slug.unS)) <$> readIORef Db.allPosts
  post <- maybe (throwError err400{errBody = "Post not found"}) pure mPost
  commentParent <-
    traverse
      (\parentSlug -> do
        mParentComment <- liftIO $ find ((parentSlug ==) . (.slug.unS)) <$> readIORef Db.allComments
        maybe
          
          (throwError err400{errBody = "Parent comment not found"})
          (pure . (.id.unS))
          mParentComment
      )
      request.parent.unS
  commentId <- liftIO newCommentId
  commentSlug <- liftIO newCommentSlug
  liftIO $
    modifyIORef Db.allComments
    (Comment
      { id = S commentId
      , slug = S commentSlug
      , parent = S commentParent
      , post = post.id
      , content = request.content
      } :) 
  pure CreatedComment{slug = S commentSlug}
  where
    newCommentId =
      CommentId . length <$> readIORef Db.allComments
    
    newCommentSlug =
      CommentSlug . fromString <$> replicateM 6 (randomRIO ('a', 'z'))
