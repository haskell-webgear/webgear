{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Copyright        : (c) Raghu Kaippully, 2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
module WebGear.Server.Middleware.Header where

import Control.Arrow (arr, returnA, (>>>))
import Data.ByteString.Conversion (ToByteString, toByteString')
import qualified Data.HashMap.Strict as HM
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Text (Text)
import Data.Void (Void)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.HTTP.Types (HeaderName)
import Web.HttpApiData (FromHttpApiData, parseHeader)
import WebGear.Core.Middleware.Header (Header (..), HeaderNotFound (..), HeaderParseError (..))
import WebGear.Core.Modifiers
import WebGear.Core.Request (Request, requestHeader)
import WebGear.Core.Response (Response (..))
import WebGear.Core.Trait (Get (..), Linked, Set (..), unlink)
import WebGear.Server.Handler (ServerHandler)

mkName :: KnownSymbol name => Proxy name -> HeaderName
mkName = fromString . symbolVal

extractRequestHeader :: (Monad m, KnownSymbol name, FromHttpApiData val)
                     => Proxy name
                     -> ServerHandler m (Linked ts Request) (Maybe (Either Text val))
extractRequestHeader proxy = proc req -> do
  returnA -< parseHeader <$> requestHeader (mkName proxy) (unlink req)

instance (Monad m, KnownSymbol name, FromHttpApiData val) => Get (ServerHandler m) (Header Required Strict name val) Request where
  {-# INLINEABLE getTrait #-}
  getTrait :: Header Required Strict name val
           -> ServerHandler m (Linked ts Request) (Either (Either HeaderNotFound HeaderParseError) val)
  getTrait Header = extractRequestHeader (Proxy @name) >>> arr f
    where
      f = \case
        Nothing        -> Left $ Left HeaderNotFound
        Just (Left e)  -> Left $ Right $ HeaderParseError e
        Just (Right x) -> Right x

instance (Monad m, KnownSymbol name, FromHttpApiData val) => Get (ServerHandler m) (Header Optional Strict name val) Request where
  {-# INLINEABLE getTrait #-}
  getTrait :: Header Optional Strict name val
           -> ServerHandler m (Linked ts Request) (Either HeaderParseError (Maybe val))
  getTrait Header = extractRequestHeader (Proxy @name) >>> arr f
    where
      f = \case
        Nothing        -> Right Nothing
        Just (Left e)  -> Left $ HeaderParseError e
        Just (Right x) -> Right $ Just x

instance (Monad m, KnownSymbol name, FromHttpApiData val) => Get (ServerHandler m) (Header Required Lenient name val) Request where
  {-# INLINEABLE getTrait #-}
  getTrait :: Header Required Lenient name val
           -> ServerHandler m (Linked ts Request) (Either HeaderNotFound (Either Text val))
  getTrait Header = extractRequestHeader (Proxy @name) >>> arr f
    where
      f = \case
        Nothing        -> Left HeaderNotFound
        Just (Left e)  -> Right $ Left e
        Just (Right x) -> Right $ Right x

instance (Monad m, KnownSymbol name, FromHttpApiData val) => Get (ServerHandler m) (Header Optional Lenient name val) Request where
  {-# INLINEABLE getTrait #-}
  getTrait :: Header Optional Lenient name val
           -> ServerHandler m (Linked ts Request) (Either Void (Maybe (Either Text val)))
  getTrait Header = extractRequestHeader (Proxy @name) >>> arr f
    where
      f = \case
        Nothing        -> Right Nothing
        Just (Left e)  -> Right $ Just $ Left e
        Just (Right x) -> Right $ Just $ Right x

instance (Monad m, KnownSymbol name, ToByteString val) => Set (ServerHandler m) (Header Required Strict name val) Response where
  {-# INLINEABLE setTrait #-}
  setTrait :: Header Required Strict name val
           -> (Linked ts Response -> Response -> val -> Linked (Header Required Strict name val : ts) Response)
           -> ServerHandler m (Linked ts Response, val) (Linked (Header Required Strict name val : ts) Response)
  setTrait Header f = proc (l, val) -> do
    let response@Response{..} = unlink l
        response' = response{responseHeaders = HM.insert (mkName $ Proxy @name) (toByteString' val) responseHeaders}
    returnA -< f l response' val

instance (Monad m, KnownSymbol name, ToByteString val) => Set (ServerHandler m) (Header Optional Strict name val) Response where
  {-# INLINEABLE setTrait #-}
  setTrait :: Header Optional Strict name val
           -> (Linked ts Response -> Response -> Maybe val -> Linked (Header Optional Strict name val : ts) Response)
           -> ServerHandler m (Linked ts Response, Maybe val) (Linked (Header Optional Strict name val : ts) Response)
  setTrait Header f = proc (l, maybeVal) -> do
    let response@Response{..} = unlink l
        response' = response{responseHeaders = HM.alter (const $ toByteString' <$> maybeVal) (mkName $ Proxy @name) responseHeaders}
    returnA -< f l response' maybeVal
