module Form where

import Data.Functor.Product (Product)
import Encode (Encoder, SimpleEncoder)
import qualified Encode as Encoder
import Decode (Decoder, SimpleDecoder, DecodeError)
import qualified Decode as Decoder
import Data.Functor.Identity (Identity (..))
import Data.Functor.Contravariant.Barbie (ContraversableB, bcontraverse)
import Data.Functor.Barbie (TraversableB, bsequence', btraverse, bmap, FunctorB)
import Data.Functor.Contravariant (contramap)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)

data Form a = Form (Encoder a) (Decoder a)

data SimpleField a = SimpleField (SimpleEncoder a) (SimpleDecoder a)

string :: SimpleField String
string = SimpleField Encoder.string Decoder.string 

int :: SimpleField Int
int = SimpleField Encoder.int Decoder.int

simple :: String -> SimpleField a -> Form a
simple key (SimpleField enc dec) = Form (Encoder.simple key enc) (Decoder.simple key dec)

compound :: String -> Form a -> Form a
compound key (Form enc dec) = Form (Encoder.compound key enc) (Decoder.compound key dec)

nested :: FunctorB shape => String -> shape Form -> shape Form
nested key =
  bmap (\(Form enc dec) -> Form (Encoder.compound key enc) (Decoder.compound key dec))

list :: Form a -> Form [a]
list (Form enc dec) = Form (Encoder.list enc) (Decoder.list dec)

toForm :: (TraversableB shape, ContraversableB shape) => shape Form -> Form (shape Identity)
toForm input =
  Form
    (bcontraverse (\(Form enc _) -> contramap runIdentity enc) input)
    (btraverse (\(Form _ dec) -> fmap Identity dec) input)

encode :: Form a -> a -> Map String (NonEmpty String)
encode (Form enc _) = Encoder.encode enc

decode :: Form a -> Map String (NonEmpty String) -> Either DecodeError (Map String (NonEmpty String), a)
decode (Form _ dec) = Decoder.decode dec
