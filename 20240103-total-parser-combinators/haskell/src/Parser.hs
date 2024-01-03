{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import Prelude hiding (drop)

import Control.Applicative (Alternative (..))
import GHC.Stack (HasCallStack)

data Nat = Z | S Nat
  deriving (Show)

plus :: Nat -> Nat -> Nat
plus Z a = a
plus (S a) b = S (plus a b)

drop :: Nat -> [a] -> [a]
drop Z xs = xs
drop (S n) [] = []
drop (S n) (x : xs) = drop n xs

-- | @undrop (drop n input) input == n@
undrop :: (HasCallStack) => [a] -> [a] -> Nat
undrop a b = go a b
 where
  go [] [] = Z
  go [] (x : xs) = S (go [] xs)
  go (x : xs) [] = error "trying to increase size by dropping"
  go (x : xs) (y : ys) = go xs ys

newtype Parser a = Parser {unParser :: String -> (Nat, Maybe a)}
  deriving (Functor)

instance Applicative Parser where
  pure a = Parser $ \input -> (Z, Just a)
  Parser pf <*> Parser pa = Parser $ \input ->
    let (n, mf) = pf input
     in case mf of
          Nothing ->
            (n, Nothing)
          Just f ->
            let (n', ma) = pa (drop n input)
             in (n `plus` n', f <$> ma)

instance Monad Parser where
  Parser pa >>= f = Parser $ \input ->
    let (n, ma) = pa input
     in case ma of
          Nothing ->
            (n, Nothing)
          Just a ->
            let (n', b) = unParser (f a) (drop n input)
             in (n `plus` n', b)

instance Alternative Parser where
  empty = Parser $ \_ -> (Z, Nothing)
  Parser pa <|> Parser pb = Parser $ \input ->
    case pa input of
      (Z, Nothing) ->
        pb input
      a ->
        a

  some pa = fixP (\self -> (:) <$> pa <*> (self <|> pure []))

  many pa = fixP (\self -> ((:) <$> pa <*> self) <|> pure [])

char :: Char -> Parser ()
char c = Parser $ \input ->
  case input of
    [] ->
      (Z, Nothing)
    x : xs ->
      if x == c
        then (S Z, Just ())
        else (Z, Nothing)

string :: String -> Parser ()
string s = Parser $ go s Z
 where
  go :: String -> Nat -> String -> (Nat, Maybe ())
  go [] n input = (n, Just ())
  go (x : xs) n [] = (Z, Nothing)
  go (x : xs) n (i : is) =
    if x == i
      then go xs (S n) is
      else (Z, Nothing)

eof :: Parser ()
eof = Parser $ \input ->
  case input of
    [] -> (Z, Just ())
    _ : _ -> (Z, Nothing)

fixP :: forall a. (Parser a -> Parser a) -> Parser a
fixP f = Parser (go Z)
 where
  go :: Nat -> String -> (Nat, Maybe a)
  go (S _) [] = (Z, Nothing)
  go (S n) (x : xs) = go n xs
  go Z input@[] =
    unParser
      (f (Parser (\input' -> let !_ = undrop input' [] in undefined)))
      input
  go Z input@(x : xs) =
    unParser
      (f (Parser (\input' -> go (undrop input' xs) xs)))
      input

fixPF :: forall a b. ((a -> Parser b) -> (a -> Parser b)) -> (a -> Parser b)
fixPF f pa = Parser (go pa Z)
 where
  go :: a -> Nat -> String -> (Nat, Maybe b)
  go a (S _) [] = (Z, Nothing)
  go a (S n) (x : xs) = go a n xs
  go a Z input@[] =
    unParser
      (f (\a' -> Parser (\input' -> let !_ = undrop input' [] in undefined)) a)
      input
  go a Z input@(x : xs) =
    unParser
      (f (\a' -> Parser (\input' -> go a' (undrop input' xs) xs)) a)
      input

fixP2 :: forall a b. ((Parser a, Parser b) -> (Parser a, Parser b)) -> (Parser a, Parser b)
fixP2 f = (Parser (go1 Z), Parser (go2 Z))
 where
  go1 :: Nat -> String -> (Nat, Maybe a)
  go1 (S _) [] = (Z, Nothing)
  go1 (S n) (x : xs) = go1 n xs
  go1 Z input@[] =
    unParser
      ( fst
          $ f
            ( Parser (\input' -> let !_ = undrop input' [] in undefined)
            , Parser (go2 Z)
            )
      )
      input
  go1 Z input@(x : xs) =
    unParser
      ( fst
          $ f
            ( Parser (\input' -> go1 (undrop input' xs) xs)
            , Parser (go2 Z)
            )
      )
      input

  go2 :: Nat -> String -> (Nat, Maybe b)
  go2 (S _) [] = (Z, Nothing)
  go2 (S n) (x : xs) = go2 n xs
  go2 Z input@[] =
    unParser
      ( snd
          $ f
            ( Parser (go1 Z)
            , Parser (\input' -> let !_ = undrop input' [] in undefined)
            )
      )
      input
  go2 Z input@(x : xs) =
    unParser
      ( snd
          $ f
            ( Parser (go1 Z)
            , Parser (\input' -> go2 (undrop input' xs) xs)
            )
      )
      input

fixPF2 ::
  forall a b c d.
  ((a -> Parser b, c -> Parser d) -> (a -> Parser b, c -> Parser d)) ->
  (a -> Parser b, c -> Parser d)
fixPF2 f = (Parser . go1 Z, Parser . go2 Z)
 where
  go1 :: Nat -> a -> String -> (Nat, Maybe b)
  go1 (S _) a [] = (Z, Nothing)
  go1 (S n) a (x : xs) = go1 n a xs
  go1 Z a input@[] =
    unParser
      ( fst
          ( f
              ( \a' -> Parser (\input' -> let !_ = undrop input' [] in undefined)
              , Parser . go2 Z
              )
          )
          a
      )
      input
  go1 Z a input@(x : xs) =
    unParser
      ( fst
          ( f
              ( \a' -> Parser (\input' -> go1 (undrop input' xs) a' xs)
              , Parser . go2 Z
              )
          )
          a
      )
      input

  go2 :: Nat -> c -> String -> (Nat, Maybe d)
  go2 (S _) c [] = (Z, Nothing)
  go2 (S n) c (x : xs) = go2 n c xs
  go2 Z c input@[] =
    unParser
      ( snd
          ( f
              ( Parser . go1 Z
              , \c' -> Parser (\input' -> let !_ = undrop input' [] in undefined)
              )
          )
          c
      )
      input
  go2 Z c input@(x : xs) =
    unParser
      ( snd
          ( f
              ( Parser . go1 Z
              , \c' -> Parser (\input' -> go2 (undrop input' xs) c' xs)
              )
          )
          c
      )
      input

some', many' :: Parser a -> Parser [a]
(some', many') =
  fixPF2
    ( \(some_self, many_self) ->
        ( \pa -> (:) <$> pa <*> many_self pa
        , \pa -> some_self pa <|> pure []
        )
    )