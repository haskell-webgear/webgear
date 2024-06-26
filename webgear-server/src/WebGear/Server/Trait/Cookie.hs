{-# OPTIONS_GHC -Wno-orphans #-}

-- | Server implementation of the `Cookie` and `SetCookie` traits.
module WebGear.Server.Trait.Cookie () where

import Control.Arrow (arr, returnA, (>>>))
import Data.Binary.Builder (toLazyByteString)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.HTTP.Types (Header, ResponseHeaders)
import qualified Web.Cookie as Cookie
import Web.HttpApiData (FromHttpApiData, parseHeader)
import WebGear.Core.Modifiers
import WebGear.Core.Request (Request, requestHeader)
import WebGear.Core.Response (Response (..))
import WebGear.Core.Trait (Get (..), Set (..), With, unwitness)
import WebGear.Core.Trait.Cookie (Cookie (..), CookieNotFound (..), CookieParseError (..), SetCookie (..))
import WebGear.Server.Handler (ServerHandler)

instance (Monad m, KnownSymbol name, FromHttpApiData val) => Get (ServerHandler m) (Cookie Required name val) where
  {-# INLINE getTrait #-}
  getTrait ::
    Cookie Required name val ->
    ServerHandler m (Request `With` ts) (Either (Either CookieNotFound CookieParseError) val)
  getTrait Cookie = extractCookie (Proxy @name) >>> arr f
    where
      f = \case
        Nothing -> Left $ Left CookieNotFound
        Just (Left e) -> Left $ Right $ CookieParseError e
        Just (Right x) -> Right x

instance (Monad m, KnownSymbol name, FromHttpApiData val) => Get (ServerHandler m) (Cookie Optional name val) where
  {-# INLINE getTrait #-}
  getTrait ::
    Cookie Optional name val ->
    ServerHandler m (Request `With` ts) (Either CookieParseError (Maybe val))
  getTrait Cookie = extractCookie (Proxy @name) >>> arr f
    where
      f = \case
        Nothing -> Right Nothing
        Just (Left e) -> Left $ CookieParseError e
        Just (Right x) -> Right $ Just x

extractCookie ::
  (Monad m, KnownSymbol name, FromHttpApiData val) =>
  Proxy name ->
  ServerHandler m (Request `With` ts) (Maybe (Either Text val))
extractCookie proxy = proc req -> do
  let cookieName :: ByteString = fromString $ symbolVal proxy

      lookupCookie :: Maybe ByteString
      lookupCookie = do
        hdr <- requestHeader "Cookie" (unwitness req)
        let cookies :: Cookie.Cookies = Cookie.parseCookies hdr
        lookup cookieName cookies

  returnA -< parseHeader <$> lookupCookie

instance (Monad m, KnownSymbol name) => Set (ServerHandler m) (SetCookie Required name) where
  {-# INLINE setTrait #-}
  setTrait ::
    SetCookie Required name ->
    (Response `With` ts -> Response -> Cookie.SetCookie -> Response `With` (SetCookie Required name : ts)) ->
    ServerHandler m (Response `With` ts, Cookie.SetCookie) (Response `With` (SetCookie Required name : ts))
  setTrait SetCookie f = proc (l, cookie) -> do
    let cookieName :: ByteString = fromString $ symbolVal $ Proxy @name
        response = unwitness l
        response' =
          case response of
            Response status hdrs body -> Response status (("Set-Cookie", cookieToBS cookieName cookie) : hdrs) body
            _ -> response
    returnA -< f l response' cookie

instance (Monad m, KnownSymbol name) => Set (ServerHandler m) (SetCookie Optional name) where
  {-# INLINE setTrait #-}
  -- If the optional value is 'Nothing', the cookie is removed from the response
  setTrait ::
    SetCookie Optional name ->
    (Response `With` ts -> Response -> Maybe Cookie.SetCookie -> Response `With` (SetCookie Optional name : ts)) ->
    ServerHandler m (Response `With` ts, Maybe Cookie.SetCookie) (Response `With` (SetCookie Optional name : ts))
  setTrait SetCookie f = proc (l, maybeCookie) -> do
    let cookieName :: ByteString = fromString $ symbolVal $ Proxy @name
        response = unwitness l
        response' =
          case response of
            Response status hdrs body -> Response status (alterCookie cookieName maybeCookie hdrs) body
            _ -> response
    returnA -< f l response' maybeCookie

alterCookie :: ByteString -> Maybe Cookie.SetCookie -> ResponseHeaders -> ResponseHeaders
alterCookie name (Just cookie) hdrs = ("Set-Cookie", cookieToBS name cookie) : hdrs
alterCookie name Nothing hdrs = filter (not . isMatchingCookie) hdrs
  where
    isMatchingCookie :: Header -> Bool
    isMatchingCookie (hdrName, hdrVal) =
      (hdrName == "Set-Cookie")
        && (name == Cookie.setCookieName (Cookie.parseSetCookie hdrVal))

cookieToBS :: ByteString -> Cookie.SetCookie -> ByteString
cookieToBS name cookie =
  toStrict $
    toLazyByteString $
      Cookie.renderSetCookie $
        cookie{Cookie.setCookieName = name}
