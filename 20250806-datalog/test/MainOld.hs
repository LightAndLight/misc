{-# language OverloadedStrings #-}
{-# language OverloadedLists #-}
{-# language DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
module MainOld where

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Json
import Data.Text (Text)
import GHC.Generics (Generic)
import Test.Hspec (hspec, describe, it, shouldBe)
import qualified Data.Vector as Vector
import LibOld
import qualified Data.Map as Map

data Movie
  = Movie
  { title :: Text
  , director :: Text
  , actor :: Text
  , theatre :: Text
  , city :: Text
  } deriving (Show, Eq, Generic, FromJSON)

loadMovies :: IO ([Movie], Table)
loadMovies = do
  Just movies <- Json.decodeFileStrict' @[Movie] "test/movies.json"
  pure
    ( movies
    , Table $
      Vector.fromList
        [ [ VString $ title m
          , VString $ director m
          , VString $ actor m
          , VString $ theatre m
          , VString $ city m
          ]
        | m <- movies
        ]
    )

main :: IO ()
main = do
  (movies, moviesTable) <- loadMovies
  hspec $ do
    describe "conjunctive queries" $ do
      it "movie(_, director, _, _, _)" $ do
        let expr = Relation "movie" [Wild, Name "director", Wild, Wild, Wild]
              
        let expected = [ Map.singleton "director" (VString $ director m) | m <- movies ]
        let actual = eval (Map.singleton "movie" moviesTable) expr
        actual `shouldBe` expected

      it "movie(\"Avatar\", director, _, _, _)" $ do
        let expr = Relation "movie" [String "Avatar", Name "director", Wild, Wild, Wild]
              
        let expected = [ Map.singleton "director" (VString $ director m) | m <- movies, title m == "Avatar" ]
        let actual = eval (Map.singleton "movie" moviesTable) expr
        actual `shouldBe` expected

      it "title = \"Avatar\", movie(title, director, _, _, _)" $ do
        let expr = Conj (Name "title" `Eq` String "Avatar") (Relation "movie" [Name "title", Name "director", Wild, Wild, Wild])
              
        let expected = [ [("director", VString $ director m), ("title", VString $ title m)] | m <- movies, title m == "Avatar" ]
        let actual = eval (Map.singleton "movie" moviesTable) expr
        actual `shouldBe` expected

      it "movie(title, director, _, _, _), title = \"Avatar\"" $ do
        let expr = Conj (Relation "movie" [Name "title", Name "director", Wild, Wild, Wild]) (Name "title" `Eq` String "Avatar")
              
        let expected = [ [("director", VString $ director m), ("title", VString $ title m)] | m <- movies, title m == "Avatar" ]
        let actual = eval (Map.singleton "movie" moviesTable) expr
        actual `shouldBe` expected

      it "movie(title, director, _, _, _), (director = \"James Cameron\" | director = \"Christopher Nolan\")" $ do
        let
          expr =
            Conj
              (Relation "movie" [Name "title", Name "director", Wild, Wild, Wild])
              (Disj
                (Name "director" `Eq` String "James Cameron")
                (Name "director" `Eq` String "Christopher Nolan")
              )
              
        let
          expected =
            [ [("director", VString $ director m), ("title", VString $ title m)]
            | m <- movies
            , director m == "James Cameron" || director m == "Christopher Nolan"
            ]
        let actual = eval (Map.singleton "movie" moviesTable) expr
        actual `shouldBe` expected

      it "(movie(title, director, _, _, _), director = \"James Cameron\") | (movie(title, director, _, _, _), director = \"Christopher Nolan\")" $ do
        let
          expr =            
            Disj
              (Conj (Relation "movie" [Name "title", Name "director", Wild, Wild, Wild]) (Name "director" `Eq` String "James Cameron"))
              (Conj (Relation "movie" [Name "title", Name "director", Wild, Wild, Wild]) (Name "director" `Eq` String "Christopher Nolan"))
              
        let
          expected =
            [ [("director", VString $ director m), ("title", VString $ title m)]
            | m <- movies
            , director m == "James Cameron"
            ] ++
            [ [("director", VString $ director m), ("title", VString $ title m)]
            | m <- movies
            , director m == "Christopher Nolan"
            ]
        let actual = eval (Map.singleton "movie" moviesTable) expr
        actual `shouldBe` expected

      it "a = \"hello\", b = \"goodbye\", a != b" $ do
        let
          expr =            
            Conj (Name "a" `Eq` String "hello") $
            Conj (Name "b" `Eq` String "goodbye") $
            Neq (Name "a") (Name "b")
             
        let
          expected =
            [ [ ("a", VString "hello")
              , ("b", VString "goodbye")
              ]
            ]
        let actual = eval (Map.singleton "movie" moviesTable) expr
        actual `shouldBe` expected

      it "movie(title1, _, actor, _, _), movie(title2, _, actor, _, _), title1 != title2" $ do
        let
          expr =            
            Conj (Relation "movie" [Name "title1", Wild, Name "actor", Wild, Wild]) $
            Conj (Relation "movie" [Name "title2", Wild, Name "actor", Wild, Wild]) $
            Neq (Name "title1") (Name "title2")
             
        let
          expected =
            [ [ ("title1", VString $ title m1)
              , ("title2", VString $ title m2)
              , ("actor", VString $ actor m1)
              ]
            | m1 <- movies
            , m2 <- filter (\m -> actor m1 == actor m) movies
            , title m1 /= title m2
            ]
        let actual = eval (Map.singleton "movie" moviesTable) expr
        actual `shouldBe` expected
