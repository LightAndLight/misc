{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Lib
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main =
  hspec $ do
    describe "lowestCommonAncestor" $ do
      it "1" $ do
        {-
          x
         / \
        y   z
        -}
        lowestCommonAncestor
          ( Map.fromList
              [ (StateId 0, Start "x")
              , (StateId 1, Change (Set.singleton $ StateId 0) (Set "y"))
              , (StateId 2, Change (Set.singleton $ StateId 0) (Set "z"))
              ]
          )
          (Set.fromList [StateId 1, StateId 2])
          `shouldBe` Just (StateId 0)

      it "2" $ do
        {-
          x   w
         / \ /
        y   z
        -}
        lowestCommonAncestor
          ( Map.fromList
              [ (StateId 0, Start "x")
              , (StateId 1, Start "w")
              , (StateId 2, Change (Set.singleton $ StateId 0) (Set "y"))
              , (StateId 3, Change (Set.fromList [StateId 0, StateId 1]) (Set "z"))
              ]
          )
          (Set.fromList [StateId 2, StateId 3])
          `shouldBe` Nothing

      it "3" $ do
        {-
            s
           / \
          x   w
         / \ /
        y   z
        -}
        lowestCommonAncestor
          ( Map.fromList
              [ (StateId 4, Start "s")
              , (StateId 0, Change (Set.singleton $ StateId 4) (Set "x"))
              , (StateId 1, Change (Set.singleton $ StateId 4) (Set "w"))
              , (StateId 2, Change (Set.singleton $ StateId 0) (Set "y"))
              , (StateId 3, Change (Set.fromList [StateId 0, StateId 1]) (Set "z"))
              ]
          )
          (Set.fromList [StateId 2, StateId 3])
          `shouldBe` Just (StateId 4)

    describe "longestCommonPath" $ do
      it "1" $ do
        longestCommonPath [Seq.fromList [2 :: Int, 1], Seq.fromList [3, 1]] `shouldBe` Seq.singleton 1
      it "2" $ do
        longestCommonPath
          [ Seq.fromList [2 :: Int, 1]
          , Seq.fromList [3, 2, 1]
          ]
          `shouldBe` Seq.fromList [2, 1]

    describe "Versioned" $ do
      describe "set" $ do
        it "simple" $ do
          let v1 = versionedNew (0 :: Int) & versionedChange (Set 5)

          let id0 = mkInitialStateId (0 :: Int)
          let id5from0 = mkStateId (Set.fromList [id0]) (Set (5 :: Int))
          v1
            `shouldBe` Versioned
              { current = Map.fromList [(id5from0, 5)]
              , history =
                  Map.fromList
                    [ (id0, Start 0)
                    , (id5from0, Change (Set.singleton id0) (Set 5))
                    ]
              }

          versionedHistory v1
            `shouldBe` (Nil `Sequential` (Set 0, id0) `Sequential` (Set 5, id5from0))

        it "reset" $ do
          let v1 = versionedNew (0 :: Int) & versionedChange (Set 5) & versionedChange (Set 0)

          let id0 = mkInitialStateId (0 :: Int)
          let id5from0 = mkStateId (Set.fromList [id0]) (Set (5 :: Int))
          let id0from5from0 = mkStateId (Set.fromList [id5from0]) (Set (0 :: Int))
          v1
            `shouldBe` Versioned
              { current = Map.fromList [(id0from5from0, 0)]
              , history =
                  Map.fromList
                    [ (id0, Start 0)
                    , (id5from0, Change (Set.singleton id0) (Set 5))
                    , (id0from5from0, Change (Set.singleton id5from0) (Set 0))
                    ]
              }

          versionedHistory v1
            `shouldBe` ( Nil
                          `Sequential` (Set 0, id0)
                          `Sequential` (Set 5, id5from0)
                          `Sequential` (Set 0, id0from5from0)
                       )

      describe "merge" $ do
        {-
        5  10
        -}
        it "conflict at creation" $ do
          let v1 = versionedNew (5 :: Int)
          let v2 = versionedNew (10 :: Int)
          let v3 = versionedMerge v1 v2

          let id5 = mkInitialStateId (5 :: Int)
          let id10 = mkInitialStateId (10 :: Int)
          v3
            `shouldBe` Versioned
              { current = Map.fromList [(id5, 5), (id10, 10)]
              , history =
                  Map.fromList
                    [ (id5, Start 5)
                    , (id10, Start 10)
                    ]
              }

          versionedHistory v3
            `shouldBe` ( Nil
                          `Concurrent` [ Nil `Sequential` (Set 10, id10)
                                       , Nil `Sequential` (Set 5, id5)
                                       ]
                       )

        it "conflicting changes" $ do
          {-
            0
           / \
          5  10
          -}
          let v1 = versionedNew (0 :: Int) & versionedChange (Set 5)
          let v2 = versionedNew (0 :: Int) & versionedChange (Set 10)

          let v3 = versionedMerge v1 v2

          let id0 = mkInitialStateId (0 :: Int)
          let id5from0 = mkStateId (Set.fromList [id0]) (Set (5 :: Int))
          let id10from0 = mkStateId (Set.fromList [id0]) (Set (10 :: Int))
          v3
            `shouldBe` Versioned
              { current = Map.fromList [(id5from0, 5), (id10from0, 10)]
              , history =
                  Map.fromList
                    [ (id0, Start 0)
                    , (id5from0, Change (Set.singleton id0) (Set 5))
                    , (id10from0, Change (Set.singleton id0) (Set 10))
                    ]
              }

          (pathsToRoot v3 =<< Map.keys (current v3))
            `shouldBe` [ Seq.fromList [id10from0, id0]
                       , Seq.fromList [id5from0, id0]
                       ]

          versionedHistory v3
            `shouldBe` ( Nil
                          `Sequential` (Set 0, id0)
                          `Concurrent` [ Nil `Sequential` (Set 10, id10from0)
                                       , Nil `Sequential` (Set 5, id5from0)
                                       ]
                       )

        it "resolved conflicting changes by replacement" $ do
          {-
            0
           / \
          5  10
           \ /
            15
          -}
          let v1 = versionedNew (0 :: Int) & versionedChange (Set 5)
          let v2 = versionedNew (0 :: Int) & versionedChange (Set 10)
          let v3 = versionedMerge v1 v2
          let v4 = v3 & versionedChange (Set 15)

          let id0 = mkInitialStateId (0 :: Int)
          let id5from0 = mkStateId (Set.fromList [id0]) (Set (5 :: Int))
          let id10from0 = mkStateId (Set.fromList [id0]) (Set (10 :: Int))
          let id15from_id5from0andid10from0 = mkStateId (Set.fromList [id5from0, id10from0]) (Set (15 :: Int))
          v4
            `shouldBe` Versioned
              { current = Map.fromList [(id15from_id5from0andid10from0, 15)]
              , history =
                  Map.fromList
                    [ (id0, Start 0)
                    , (id5from0, Change (Set.singleton id0) (Set 5))
                    , (id10from0, Change (Set.singleton id0) (Set 10))
                    , (id15from_id5from0andid10from0, Change (Set.fromList [id5from0, id10from0]) (Set 15))
                    ]
              }

          versionedHistory v4
            `shouldBe` ( Nil
                          `Sequential` (Set 0, id0)
                          `Concurrent` [ Nil `Sequential` (Set 10, id10from0)
                                       , Nil `Sequential` (Set 5, id5from0)
                                       ]
                          `Sequential` (Set 15, id15from_id5from0andid10from0)
                       )

        it "resolved conflicting changes by picking" $ do
          {-
            0
           / \
          5  10
           \ /
            10
          -}
          let v1 = versionedNew (0 :: Int) & versionedChange (Set 5)
          let v2 = versionedNew (0 :: Int) & versionedChange (Set 10)
          let v3 = versionedMerge v1 v2

          let id0 = mkInitialStateId (0 :: Int)
          let id5from0 = mkStateId (Set.fromList [id0]) (Set (5 :: Int))
          let id10from0 = mkStateId (Set.fromList [id0]) (Set (10 :: Int))

          let v4 = v3 & versionedChange (Pick id10from0)
          let id10from_id5from0andid10from0 = mkStateId (Set.fromList [id5from0, id10from0]) (Pick @Int id10from0)
          v4
            `shouldBe` Versioned
              { current = Map.fromList [(id10from_id5from0andid10from0, 10)]
              , history =
                  Map.fromList
                    [ (id0, Start 0)
                    , (id5from0, Change (Set.singleton id0) (Set 5))
                    , (id10from0, Change (Set.singleton id0) (Set 10))
                    , (id10from_id5from0andid10from0, Change (Set.fromList [id5from0, id10from0]) (Pick id10from0))
                    ]
              }

          versionedHistory v4
            `shouldBe` ( Nil
                          `Sequential` (Set 0, id0)
                          `Concurrent` [ Nil `Sequential` (Set 10, id10from0)
                                       , Nil `Sequential` (Set 5, id5from0)
                                       ]
                          `Sequential` (Pick id10from0, id10from_id5from0andid10from0)
                       )
