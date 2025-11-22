{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pretty where

import Data.DList (DList)
import qualified Data.DList as DList
import Data.List (unsnoc)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy (LazyText)
import qualified Data.Text.Lazy as LazyText

newtype Doc = Doc (DList Text)
  deriving (Semigroup, Monoid)

render :: Doc -> LazyText
render (Doc ls) =
  foldMap ((<> fromString "\n") . LazyText.fromStrict) (DList.toList ls)

class AsLine s where line :: s -> Doc

instance AsLine String where line = Doc . DList.singleton . fromString
instance AsLine Text where line = Doc . DList.singleton

indented :: Int -> Doc -> Doc
indented n (Doc ls) = Doc (fmap (Text.replicate n (fromString " ") <>) ls)

{-| Join the last line of the first @Doc@ with the first line of the second @Doc@

Example:

@
'append' ('line' "a" <> 'line' "b") ('line' "c" <> 'line' "d")
==
'line' "a" <> 'line' "bc" <> 'line' "d"
@
-}
extend :: Doc -> Doc -> Doc
extend (Doc ls) (Doc ls') =
  case (unsnoc $ DList.toList ls, DList.toList ls') of
    (Nothing, []) ->
      Doc mempty
    (Just{}, []) ->
      Doc ls
    (Nothing, _ : _) ->
      Doc ls'
    (Just (start, l), l' : rest') ->
      Doc (DList.fromList start <> DList.singleton (l <> l') <> DList.fromList rest')
