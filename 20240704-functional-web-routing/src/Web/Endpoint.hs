{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Web.Endpoint
  ( EndpointFor
  , Endpoint(..)
  , mkEndpoint
  )
where

import Data.Kind (Type)
import Web.Body (Body )
import Web.Param (Param)
import Web.Response (Response )

type family EndpointFor (m :: Type -> Type) (a :: Type) :: Type where
  EndpointFor m (Param a -> b) = a -> EndpointFor m b
  EndpointFor m (Body a -> b) = a -> EndpointFor m b
  EndpointFor m (Response a) = m (Response a)

newtype Endpoint m a = Endpoint { runEndpoint :: EndpointFor m a }

mkEndpoint :: EndpointFor m a -> Endpoint m a
mkEndpoint = Endpoint
