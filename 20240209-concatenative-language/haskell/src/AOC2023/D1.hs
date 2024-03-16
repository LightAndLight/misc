{-# LANGUAGE NoImplicitPrelude #-}

module AOC2023.D1 where

import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import Lib
import Lib.Prelude
import Lib.Ty
import Prelude (IO, ($))

getInput :: IO Text
getInput = Text.IO.readFile "src/AOC2023/d1.txt"

part1_testInput :: Text
part1_testInput = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

part2_testInput :: Text
part2_testInput = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"

{-# INLINEABLE sumLineDigits #-}
sumLineDigits ::
  (Cat t, CtxC t ctx) =>
  (forall x. (CtxC t x) => t (TList TChar ': x) (TList TInt ': x)) ->
  t (TString ': ctx) (TInt ': ctx)
sumLineDigits getDigits =
  sum
    . map
    . fn
      ( add
          . par (mul . int 10 . first) last
          . getDigits
          . chars
          . var Z
      )
    . splits
    . char '\n'

getDigitsPart1 :: (Cat t, CtxC t ctx) => t (TList TChar ': ctx) (TList TInt ': ctx)
getDigitsPart1 =
  filterMap . fn (decimalDigit . var Z)

part1 :: (Cat t, CtxC t ctx) => t (TString ': ctx) (TInt ': ctx)
part1 = sumLineDigits getDigitsPart1

{-# INLINEABLE getDigitsPart2 #-}
getDigitsPart2 :: (Cat t, CtxC t ctx) => t (TList TChar ': ctx) (TList TInt ': ctx)
getDigitsPart2 =
  fix
    ( \self ->
        bind $ \xs ->
          matchList
            nil
            ( bind
                ( \xs' ->
                    matchMaybe (self . xs') (bind $ \n -> cons . n . self . xs')
                      . digitPrefix
                      . xs
                )
                . drop
            )
            . xs
    )

digitPrefix :: (Cat t, CtxC t ctx) => t (TList TChar ': ctx) (TMaybe TInt ': ctx)
digitPrefix =
  bind $ \xs ->
    (foldr . fn (orElse . unpair . var Z) . nothing)
      . (map . fn (ifte just (nothing . drop) . bind (\prefix -> isPrefixOf . chars . prefix . xs) . unpair . var Z))
      . ( cons
            . (pair . string "0" . int 0)
            . cons
            . (pair . string "1" . int 1)
            . cons
            . (pair . string "2" . int 2)
            . cons
            . (pair . string "3" . int 3)
            . cons
            . (pair . string "4" . int 4)
            . cons
            . (pair . string "5" . int 5)
            . cons
            . (pair . string "6" . int 6)
            . cons
            . (pair . string "7" . int 7)
            . cons
            . (pair . string "8" . int 8)
            . cons
            . (pair . string "9" . int 9)
            . cons
            . (pair . string "zero" . int 0)
            . cons
            . (pair . string "one" . int 1)
            . cons
            . (pair . string "two" . int 2)
            . cons
            . (pair . string "three" . int 3)
            . cons
            . (pair . string "four" . int 4)
            . cons
            . (pair . string "five" . int 5)
            . cons
            . (pair . string "six" . int 6)
            . cons
            . (pair . string "seven" . int 7)
            . cons
            . (pair . string "eight" . int 8)
            . cons
            . (pair . string "nine" . int 9)
            . nil
        )

part2 :: (Cat t, CtxC t ctx) => t (TString ': ctx) (TInt ': ctx)
part2 = sumLineDigits getDigitsPart2
