module SPWA.Element (Element (..), html) where

import SPWA.Html (Html)

data Element = MkElement String Html

html :: Element -> Html
html (MkElement _ a) = a