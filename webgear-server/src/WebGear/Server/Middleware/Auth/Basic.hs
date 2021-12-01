{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WebGear.Server.Middleware.Auth.Basic where

import Control.Arrow (arr, returnA, (>>>))
import Data.Bifunctor (first)
import Data.ByteString.Base64 (decodeLenient)
import Data.ByteString.Char8 (intercalate, split)
import Data.Void (Void)
import WebGear.Core.Handler (arrM)
import WebGear.Core.Middleware.Auth.Basic (
  BasicAuth' (..),
  BasicAuthError (..),
  Credentials (..),
  Password (..),
  Username (..),
 )
import WebGear.Core.Middleware.Auth.Common (
  AuthToken (..),
  AuthorizationHeader,
  getAuthorizationHeaderTrait,
 )
import WebGear.Core.Modifiers
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..), Linked)
import WebGear.Server.Handler (ServerHandler)

parseCreds :: AuthToken scheme -> Either (BasicAuthError e) Credentials
parseCreds AuthToken{..} =
  case split ':' (decodeLenient authToken) of
    [] -> Left BasicAuthCredsBadFormat
    u : ps -> Right $ Credentials (Username u) (Password $ intercalate ":" ps)

instance
  ( Monad m
  , Get (ServerHandler m) (AuthorizationHeader scheme) Request
  ) =>
  Get (ServerHandler m) (BasicAuth' Required scheme m e a) Request
  where
  {-# INLINEABLE getTrait #-}
  getTrait ::
    BasicAuth' Required scheme m e a ->
    ServerHandler m (Linked ts Request) (Either (BasicAuthError e) a)
  getTrait BasicAuth'{..} = proc request -> do
    result <- getAuthorizationHeaderTrait @scheme -< request
    case result of
      Nothing -> returnA -< Left BasicAuthHeaderMissing
      (Just (Left _)) -> returnA -< Left BasicAuthSchemeMismatch
      (Just (Right token)) ->
        case parseCreds token of
          Left e -> returnA -< Left e
          Right c -> validateCreds -< c
    where
      validateCreds :: ServerHandler m Credentials (Either (BasicAuthError e) a)
      validateCreds = arrM $ \creds -> do
        res <- toBasicAttribute creds
        pure $ first BasicAuthAttributeError res

instance
  ( Monad m
  , Get (ServerHandler m) (AuthorizationHeader scheme) Request
  ) =>
  Get (ServerHandler m) (BasicAuth' Optional scheme m e a) Request
  where
  {-# INLINEABLE getTrait #-}
  getTrait ::
    BasicAuth' Optional scheme m e a ->
    ServerHandler m (Linked ts Request) (Either Void (Either (BasicAuthError e) a))
  getTrait BasicAuth'{..} = getTrait (BasicAuth'{..} :: BasicAuth' Required scheme m e a) >>> arr Right
