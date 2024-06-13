{-# OPTIONS_GHC -Wno-orphans #-}

-- | Server implementation of the `QueryParam` trait.
module WebGear.Server.Trait.QueryParam () where

import Control.Arrow (arr, returnA, (>>>))
import Data.List (find)
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Text (Text)
import Data.Void (Void)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.HTTP.Types (queryToQueryText)
import Web.HttpApiData (FromHttpApiData (..))
import WebGear.Core.Modifiers
import WebGear.Core.Request (Request, queryString)
import WebGear.Core.Trait (Get (..), With, unwitness)
import WebGear.Core.Trait.QueryParam (
  ParamNotFound (..),
  ParamParseError (..),
  QueryParam (..),
 )
import WebGear.Server.Handler (ServerHandler)

extractQueryParam ::
  (Monad m, KnownSymbol name, FromHttpApiData val) =>
  Proxy name ->
  ServerHandler m (Request `With` ts) (Maybe (Either Text val))
extractQueryParam proxy = proc req -> do
  let name = fromString $ symbolVal proxy
      params = queryToQueryText $ queryString $ unwitness req
  returnA -< parseQueryParam <$> (find ((== name) . fst) params >>= snd)

instance (Monad m, KnownSymbol name, FromHttpApiData val) => Get (ServerHandler m) (QueryParam Required Strict name val) where
  {-# INLINE getTrait #-}
  getTrait ::
    QueryParam Required Strict name val ->
    ServerHandler m (Request `With` ts) (Either (Either ParamNotFound ParamParseError) val)
  getTrait QueryParam = extractQueryParam (Proxy @name) >>> arr f
    where
      f = \case
        Nothing -> Left $ Left ParamNotFound
        Just (Left e) -> Left $ Right $ ParamParseError e
        Just (Right x) -> Right x

instance (Monad m, KnownSymbol name, FromHttpApiData val) => Get (ServerHandler m) (QueryParam Optional Strict name val) where
  {-# INLINE getTrait #-}
  getTrait ::
    QueryParam Optional Strict name val ->
    ServerHandler m (Request `With` ts) (Either ParamParseError (Maybe val))
  getTrait QueryParam = extractQueryParam (Proxy @name) >>> arr f
    where
      f = \case
        Nothing -> Right Nothing
        Just (Left e) -> Left $ ParamParseError e
        Just (Right x) -> Right $ Just x

instance (Monad m, KnownSymbol name, FromHttpApiData val) => Get (ServerHandler m) (QueryParam Required Lenient name val) where
  {-# INLINE getTrait #-}
  getTrait ::
    QueryParam Required Lenient name val ->
    ServerHandler m (Request `With` ts) (Either ParamNotFound (Either Text val))
  getTrait QueryParam = extractQueryParam (Proxy @name) >>> arr f
    where
      f = \case
        Nothing -> Left ParamNotFound
        Just (Left e) -> Right $ Left e
        Just (Right x) -> Right $ Right x

instance (Monad m, KnownSymbol name, FromHttpApiData val) => Get (ServerHandler m) (QueryParam Optional Lenient name val) where
  {-# INLINE getTrait #-}
  getTrait ::
    QueryParam Optional Lenient name val ->
    ServerHandler m (Request `With` ts) (Either Void (Maybe (Either Text val)))
  getTrait QueryParam = extractQueryParam (Proxy @name) >>> arr f
    where
      f = \case
        Nothing -> Right Nothing
        Just (Left e) -> Right $ Just $ Left e
        Just (Right x) -> Right $ Just $ Right x
