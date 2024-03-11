{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -ddump-simpl
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes 
    -ddump-to-file #-}

module AOC2023.D1 where

import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import Lib
import Prelude hiding (any, drop, foldr, id, last, map, sum, (.))

getInput :: IO Text
getInput = Text.IO.readFile "src/AOC2023/d1.txt"

part1_testInput :: Text
part1_testInput = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

part2_testInput :: Text
part2_testInput = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"

{-# INLINEABLE sumLineDigits #-}
sumLineDigits ::
  (Cat t) =>
  (forall x. t (x :. TList TChar) (x :. TList TInt)) ->
  t (ctx :. TString) (ctx :. TInt)
sumLineDigits getDigits =
  char '\n'
    . splits
    . fn
      ( var Z
          . chars
          . getDigits
          . par (first . int 10 . mul) last
          . add
      )
    . map
    . sum

getDigitsPart1 :: (Cat t) => t (ctx :. TList TChar) (ctx :. TList TInt)
getDigitsPart1 =
  fn (var Z . decimalDigit) . filterMap

part1 :: (Cat t) => t (ctx :. TString) (ctx :. TInt)
part1 = sumLineDigits getDigitsPart1

{-# INLINEABLE getDigitsPart2 #-}
getDigitsPart2 :: (Cat t) => t (ctx :. TList TChar) (ctx ':. TList TInt)
getDigitsPart2 =
  fix
    ( \self ->
        bind $ \xs ->
          xs
            . matchList
              nil
              ( drop
                  . bind
                    ( \xs' ->
                        xs
                          . digitPrefix
                          . matchMaybe (xs' . self) (bind $ \n -> xs' . self . n . cons)
                    )
              )
    )

digitPrefix :: (Cat t) => t (ctx :. TList TChar) (ctx :. TMaybe TInt)
digitPrefix =
  bind $ \xs ->
    ( nil
        . (int 9 . string "9" . pair)
        . cons
        . (int 8 . string "8" . pair)
        . cons
        . (int 7 . string "7" . pair)
        . cons
        . (int 6 . string "6" . pair)
        . cons
        . (int 5 . string "5" . pair)
        . cons
        . (int 4 . string "4" . pair)
        . cons
        . (int 3 . string "3" . pair)
        . cons
        . (int 2 . string "2" . pair)
        . cons
        . (int 1 . string "1" . pair)
        . cons
        . (int 0 . string "0" . pair)
        . cons
        . (int 9 . string "nine" . pair)
        . cons
        . (int 8 . string "eight" . pair)
        . cons
        . (int 7 . string "seven" . pair)
        . cons
        . (int 6 . string "six" . pair)
        . cons
        . (int 5 . string "five" . pair)
        . cons
        . (int 4 . string "four" . pair)
        . cons
        . (int 3 . string "three" . pair)
        . cons
        . (int 2 . string "two" . pair)
        . cons
        . (int 1 . string "one" . pair)
        . cons
        . (int 0 . string "zero" . pair)
        . cons
    )
      . fn (var Z . unpair . bind (\prefix -> xs . prefix . chars) . isPrefixOf . ifte just (drop . nothing))
      . map
      . (fn (var Z . unpair . orElse) . nothing . foldr)

part2 :: (Cat t) => t (ctx :. TString) (ctx :. TInt)
part2 = sumLineDigits getDigitsPart2