{-# OPTIONS_GHC -Wno-orphans #-}

-- | Server implementation of the `Header` trait.
module WebGear.Server.Trait.Header () where

import Control.Arrow (arr, returnA, (>>>))
import Data.ByteString (ByteString)
import Data.ByteString.Conversion (ToByteString, toByteString')
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Text (Text)
import Data.Void (Void)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.HTTP.Types (HeaderName, ResponseHeaders)
import Web.HttpApiData (FromHttpApiData, parseHeader)
import WebGear.Core.Modifiers
import WebGear.Core.Request (Request, requestHeader)
import WebGear.Core.Response (Response (..))
import WebGear.Core.Trait (Get (..), Set (..), With, unwitness)
import WebGear.Core.Trait.Header (HeaderNotFound (..), HeaderParseError (..), RequestHeader (..), ResponseHeader (..))
import WebGear.Server.Handler (ServerHandler)

extractRequestHeader ::
  (Monad m, KnownSymbol name, FromHttpApiData val) =>
  Proxy name ->
  ServerHandler m (Request `With` ts) (Maybe (Either Text val))
extractRequestHeader proxy = proc req -> do
  let headerName :: HeaderName = fromString $ symbolVal proxy
  returnA -< parseHeader <$> requestHeader headerName (unwitness req)

instance (Monad m, KnownSymbol name, FromHttpApiData val) => Get (ServerHandler m) (RequestHeader Required Strict name val) Request where
  {-# INLINE getTrait #-}
  getTrait ::
    RequestHeader Required Strict name val ->
    ServerHandler m (Request `With` ts) (Either (Either HeaderNotFound HeaderParseError) val)
  getTrait RequestHeader = extractRequestHeader (Proxy @name) >>> arr f
    where
      f = \case
        Nothing -> Left $ Left HeaderNotFound
        Just (Left e) -> Left $ Right $ HeaderParseError e
        Just (Right x) -> Right x

instance (Monad m, KnownSymbol name, FromHttpApiData val) => Get (ServerHandler m) (RequestHeader Optional Strict name val) Request where
  {-# INLINE getTrait #-}
  getTrait ::
    RequestHeader Optional Strict name val ->
    ServerHandler m (Request `With` ts) (Either HeaderParseError (Maybe val))
  getTrait RequestHeader = extractRequestHeader (Proxy @name) >>> arr f
    where
      f = \case
        Nothing -> Right Nothing
        Just (Left e) -> Left $ HeaderParseError e
        Just (Right x) -> Right $ Just x

instance (Monad m, KnownSymbol name, FromHttpApiData val) => Get (ServerHandler m) (RequestHeader Required Lenient name val) Request where
  {-# INLINE getTrait #-}
  getTrait ::
    RequestHeader Required Lenient name val ->
    ServerHandler m (Request `With` ts) (Either HeaderNotFound (Either Text val))
  getTrait RequestHeader = extractRequestHeader (Proxy @name) >>> arr f
    where
      f = \case
        Nothing -> Left HeaderNotFound
        Just (Left e) -> Right $ Left e
        Just (Right x) -> Right $ Right x

instance (Monad m, KnownSymbol name, FromHttpApiData val) => Get (ServerHandler m) (RequestHeader Optional Lenient name val) Request where
  {-# INLINE getTrait #-}
  getTrait ::
    RequestHeader Optional Lenient name val ->
    ServerHandler m (Request `With` ts) (Either Void (Maybe (Either Text val)))
  getTrait RequestHeader = extractRequestHeader (Proxy @name) >>> arr f
    where
      f = \case
        Nothing -> Right Nothing
        Just (Left e) -> Right $ Just $ Left e
        Just (Right x) -> Right $ Just $ Right x

instance (Monad m, KnownSymbol name, ToByteString val) => Set (ServerHandler m) (ResponseHeader Required name val) Response where
  {-# INLINE setTrait #-}
  setTrait ::
    ResponseHeader Required name val ->
    (Response `With` ts -> Response -> val -> Response `With` (ResponseHeader Required name val : ts)) ->
    ServerHandler m (Response `With` ts, val) (Response `With` (ResponseHeader Required name val : ts))
  setTrait ResponseHeader f = proc (l, val) -> do
    let headerName :: HeaderName = fromString $ symbolVal $ Proxy @name
        response@Response{..} = unwitness l
        response' = response{responseHeaders = (headerName, toByteString' val) : responseHeaders}
    returnA -< f l response' val

instance (Monad m, KnownSymbol name, ToByteString val) => Set (ServerHandler m) (ResponseHeader Optional name val) Response where
  {-# INLINE setTrait #-}
  -- If the optional value is 'Nothing', the header is removed from the response
  setTrait ::
    ResponseHeader Optional name val ->
    (Response `With` ts -> Response -> Maybe val -> Response `With` (ResponseHeader Optional name val : ts)) ->
    ServerHandler m (Response `With` ts, Maybe val) (Response `With` (ResponseHeader Optional name val : ts))
  setTrait ResponseHeader f = proc (l, maybeVal) -> do
    let headerName :: HeaderName = fromString $ symbolVal $ Proxy @name
        response@Response{..} = unwitness l
        response' = response{responseHeaders = alterHeader headerName (toByteString' <$> maybeVal) responseHeaders}
    returnA -< f l response' maybeVal

alterHeader :: HeaderName -> Maybe ByteString -> ResponseHeaders -> ResponseHeaders
alterHeader name Nothing hdrs = filter (\(n, _) -> name /= n) hdrs
alterHeader name (Just val) hdrs = (name, val) : hdrs
