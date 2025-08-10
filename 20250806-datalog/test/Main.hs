{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Test.Hspec (hspec, describe, it, shouldBe)
import Lib
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Json
import qualified Data.Set as Set
import Data.String (fromString)

data Movie
  = Movie
  { title :: Text
  , director :: Text
  , actor :: Text
  , theatre :: Text
  , city :: Text
  } deriving (Show, Eq, Generic, FromJSON)

loadMovies :: IO [Movie]
loadMovies = do
  Just movies <- Json.decodeFileStrict' @[Movie] "test/movies.json"
  pure movies

evaluation_1_input :: Program
evaluation_1_input = Program [Rule "r" ["x"] [Relation "r" [Var "x"]]]

evaluation_2_input :: Program
evaluation_2_input =
  Program
    [ Rule "r" ["x"] [Relation "s" [Var "x"], Relation "r" [Var "x"]]
    , Fact "s" [Natural 0]
    ]

evaluation_3_input :: Program
evaluation_3_input =
  Program
    [ Rule
        "t"
        ["x", "z"]
        [ Relation "e" [Var "x", Var "y"]
        , Relation "e" [Var "y", Var "z"]
        ]
    , Fact "e" [Natural 0, Natural 1]
    , Fact "e" [Natural 1, Natural 2]
    , Fact "e" [Natural 0, Natural 3]
    , Fact "e" [Natural 2, Natural 4]
    ]

evaluation_4_input :: Program
evaluation_4_input =
  Program
    [ Rule "t" ["x", "y"] [Relation "e" [Var "x", Var "y"]]
    , Rule
        "t"
        ["x", "z"]
        [ Relation "e" [Var "x", Var "y"]
        , Relation "t" [Var "y", Var "z"]
        ]
    , Fact "e" [Natural 0, Natural 1]
    , Fact "e" [Natural 1, Natural 2]
    , Fact "e" [Natural 0, Natural 3]
    , Fact "e" [Natural 2, Natural 4]
    ]

path_program :: Program
path_program =
  Program
    [ Rule "path" ["x", "y"] [Relation "edge" [Var "x", Var "y"]]
    , Rule "path" ["x", "z"] [Relation "edge" [Var "x", Var "y"], Relation "path" [Var "y", Var "z"]]
    ]

path_db :: Database
path_db =
  Database
    [ ( "edge"
      , [ Row [String "a", String "b"]
        , Row [String "b", String "c"]
        ]
      )
    ]

path_query :: Program
path_query =
  Program
    [ Rule "query" ["x"] [Relation "path" [Constant $ String "a", Var "x"]]
    ]

magic_path_db :: Database
magic_path_db =
  Database
    [ ( "edge"
      , [ Row [String "a", String "b"]
        , Row [String "b", String "c"]
        ] <>
        Set.fromList [ Row [String "x", String . fromString $ "x" ++ show n ]| n <- [0::Int ..9]]
      )
    ]

magic_path_bf_program :: Program
magic_path_bf_program =
  Program
    [ Fact "m_path_bf" [String "a"]
    , Rule "m_path_bf" ["y"] [Relation "m_path_bf" [Var "x"], Relation "edge" [Var "x", Var "y"]]
    , Rule "path_bf" ["x", "y"] [Relation "m_path_bf" [Var "x"], Relation "edge" [Var "x", Var "y"]]
    , Rule "path_bf" ["x", "z"] [Relation "m_path_bf" [Var "x"], Relation "edge" [Var "x", Var "y"], Relation "path_bf" [Var "y", Var "z"]]
    ]

magic_path_bf_query :: Program
magic_path_bf_query =
  Program
    [ Rule "query" ["x"] [Relation "path_bf" [Constant $ String "a", Var "x"]]
    ]

magic_path_fb_program :: Program
magic_path_fb_program =
  Program
    [ Fact "m_path_fb" [String "c"]
    , Rule "m_path_fb" ["z"] [Relation "m_path_fb" [Var "z"]]
    , Rule "path_fb" ["x", "y"] [Relation "m_path_fb" [Var "y"], Relation "edge" [Var "x", Var "y"]]
    , Rule "path_fb" ["x", "z"] [Relation "m_path_fb" [Var "z"], Relation "edge" [Var "x", Var "y"], Relation "path_fb" [Var "y", Var "z"]]
    ]

main :: IO ()
main = do
  _movies <- loadMovies
  hspec $ do
    describe "datalog" $ do
      it "r(X) :- r(X)." $ do
        let input = evaluation_1_input
        let (_trace, Change actual_naive) = eval_naive databaseEmpty input
        let (_trace, Change actual_seminaive) = eval_seminaive databaseEmpty input
        let expected = databaseEmpty
        actual_naive `shouldBe` expected
        actual_seminaive `shouldBe` expected

      it "r(X) :- s(X), r(X). s(0)." $ do
        let input = evaluation_2_input
        let (_trace, Change actual_naive) = eval_naive databaseEmpty input
        let (_trace, Change actual_seminaive) = eval_seminaive databaseEmpty input
        let expected = Database [("s", [Row [Natural 0]])]
        actual_naive `shouldBe` expected
        actual_seminaive `shouldBe` expected

      it "t(X, Z) :- e(X, Y), e(Y, Z). e(0, 1). e(1, 2). e(0, 3). e(2, 4)" $ do
        let input = evaluation_3_input
        let (_trace, Change actual_naive) = eval_naive databaseEmpty input
        let (_trace, Change actual_seminaive) = eval_seminaive databaseEmpty input
        let
          expected =
            Database
              [ ( "e"
                , [ Row [Natural 0, Natural 1]
                  , Row [Natural 1, Natural 2]
                  , Row [Natural 0, Natural 3]
                  , Row [Natural 2, Natural 4]
                  ]
                )
              , ( "t"
                , [ Row [Natural 0, Natural 2]
                  , Row [Natural 1, Natural 4]
                  ]
                )
              ]
        actual_naive `shouldBe` expected
        actual_seminaive `shouldBe` expected

      it "t(X, Y) :- e(X, Y). t(X, Z) :- e(X, Y), t(Y, Z). e(0, 1). e(1, 2). e(0, 3). e(2, 4)" $ do
        let input = evaluation_4_input
        let (_trace, Change actual_naive) = eval_naive databaseEmpty input
        let (_trace, Change actual_seminaive) = eval_seminaive databaseEmpty input
        let
          expected =
            Database
              [ ( "e"
                , [ Row [Natural 0, Natural 1]
                  , Row [Natural 1, Natural 2]
                  , Row [Natural 0, Natural 3]
                  , Row [Natural 2, Natural 4]
                  ]
                )
              , ( "t"
                , [ Row [Natural 0, Natural 1]
                  , Row [Natural 1, Natural 2]
                  , Row [Natural 0, Natural 3]
                  , Row [Natural 2, Natural 4]
                  , Row [Natural 0, Natural 2]
                  , Row [Natural 0, Natural 4]
                  , Row [Natural 1, Natural 4]
                  ]
                )
              ]
        actual_naive `shouldBe` expected
        actual_seminaive `shouldBe` expected

    describe "path(X, Y) :- edge(X, Y). path(X, Z) :- edge(X, Y), path(Y, Z)" $ do
      describe "edge(\"a\", \"b\"). edge(\"b\", \"c\")." $ do
        let db = path_db
        let program = path_program

        describe "query(X) :- path(\"a\", X)" $ do
          let query = path_query
          let (_trace, Change actual_naive) = eval_naive db $ program <> query
          let (_trace, Change actual_seminaive) = eval_seminaive db $ program <> query

          let
            expected =
              Database
                [ ( "path"
                  , [ Row [String "a", String "b"]
                    , Row [String "b", String "c"]
                    , Row [String "a", String "c"]
                    ]
                  )
                , ("query", [Row [String "b"], Row [String "c"]])
                ]

          it "naive" $ actual_naive `shouldBe` expected
          it "seminaive" $ actual_seminaive `shouldBe` expected

        it "query(X) :- path(X, \"a\")" $ do
          let
            query =
              Program
              [ Rule "query" ["x"] [Relation "path" [Var "x", Constant $ String "a"]]
              ]
          let (_trace, Change actual_naive) = eval_naive db $ program <> query
          let (_trace, Change actual_seminaive) = eval_seminaive db $ program <> query

          let
            expected =
              Database
                [ ( "path"
                  , [ Row [String "a", String "b"]
                    , Row [String "b", String "c"]
                    , Row [String "a", String "c"]
                    ]
                  )
                ]

          actual_naive `shouldBe` expected
          actual_seminaive `shouldBe` expected

    describe "manual magic sets transforms" $ do
      let db = magic_path_db

      {- The following tests demonstrate the improvements provided by the "magic sets"
      transformation.

      The original program is:

      ```prolog
      path(X, Y) :- edge(X, Y).
      path(X, Z) :- edge(X, Y), path(Y, Z).
      ```

      For a query such as `query(X) :- path(a, X).` or `query(X) :- path(X, a).`,
      bottom-up evaluation of the program will produce a complete `path` relation in
      addition to the query's answers. When those answers are a small fraction of the
      total `path` relation, much work is wasted.
      -}
      describe "path_bf(X, Y) :- m_path_bf(X), edge(X, Y). path_bf(X, Z) :- m_path_bf(X), edge(X, Y), path_bf(Y, Z)" $ do

        {-
        The `edge` relation:

        ```prolog
        edge(a, b).
        edge(b, c).
        edge(x, x0).
        edge(x, x1).
        ...
        edge(x, x9)
        ```

        While answering `query(X) :- path(a, X).`, all the `edge(x, _)` tuples will be
        included in the `path` relation, which is then scanned for `path(a, _)` elements.

        The magic sets transformation of this query is:

        ```prolog
        m_path_bf(a).
        m_path_bf(Y) :- m_path_bf(X), edge(X, Y).

        path_bf(X, Y) :- m_path_bf(X), edge(X, Y).
        path_bf(X, Z) :- m_path_bf(X), edge(X, Y), path_bf(Y, Z).

        query(X) :- path_bf(a, X).
        ```

        An extra relation `m_path_bf` keeps track of relevant arguments to `path_bf`.
        `m_path_bf` then forces `path_bf` to produce only tuples that are relevant to
        answering the `query`.

        The resulting database is:

        ```
        m_path_bf = {a, b, c}
        path_bf = {(a, b), (b, c), (a, c)}
        query = {b, c}
        ```

        `path_bf` lacks the `(x, _)` tuples stored in `edge`.
        -}

        describe "edge(\"a\", \"b\"). edge(\"b\", \"c\"). edge(\"x\", ...)" $ do
          let program = magic_path_bf_program

          it "query(X) :- path_bf(\"a\", X)" $ do
            let query = magic_path_bf_query
            let (_trace, Change actual_naive) = eval_naive db $ program <> query
            let (_trace, Change actual_seminaive) = eval_seminaive db $ program <> query

            let
              expected =
                Database
                  [ ( "m_path_bf"
                    , [ Row [String "a"]
                      , Row [String "b"]
                      , Row [String "c"]
                      ]
                    )
                  , ( "path_bf"
                    , [ Row [String "a", String "b"]
                      , Row [String "b", String "c"]
                      , Row [String "a", String "c"]
                      ]
                    )
                  , ("query", [Row [String "b"], Row [String "c"]])
                  ]

            actual_naive `shouldBe` expected
            actual_seminaive `shouldBe` expected

      describe "path_fb(X, Y) :- m_path_fb(Y), edge(X, Y). path_fb(X, Z) :- m_path_fb(Z), edge(X, Y), path_fb(Y, Z)" $ do
        describe "edge(\"a\", \"b\"). edge(\"b\", \"c\"). edge(\"x\", ...)" $ do
          let program = magic_path_fb_program

          it "query(X) :- path_fb(X, \"c\")" $ do
            let
              query =
                Program
                [ Rule "query" ["x"] [Relation "path_fb" [Var "x", Constant $ String "c"]]
                ]
            let (_trace, Change actual_naive) = eval_naive db $ program <> query
            let (_trace, Change actual_seminaive) = eval_seminaive db $ program <> query

            let
              expected =
                Database
                  [ ( "m_path_fb"
                    , [ Row [String "c"]
                      ]
                    )
                  , ( "path_fb"
                    , [ Row [String "b", String "c"]
                      , Row [String "a", String "c"]
                      ]
                    )
                  , ("query", [Row [String "a"], Row [String "b"]])
                  ]

            actual_naive `shouldBe` expected
            actual_seminaive `shouldBe` expected
