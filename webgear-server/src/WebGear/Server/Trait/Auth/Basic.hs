{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Server implementation of the 'BasicAuth'' trait.
module WebGear.Server.Trait.Auth.Basic where

import Control.Arrow (arr, returnA, (>>>))
import Data.Bifunctor (first)
import Data.ByteString.Base64 (decodeLenient)
import Data.ByteString.Char8 (intercalate, split)
import Data.Void (Void)
import WebGear.Core.Handler (arrM)
import WebGear.Core.Modifiers
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..), HasTrait, With, from, pick)
import WebGear.Core.Trait.Auth.Basic (
  BasicAuth' (..),
  BasicAuthError (..),
  Credentials (..),
  Password (..),
  Username (..),
 )
import WebGear.Core.Trait.Auth.Common (
  AuthToken (..),
  AuthorizationHeader,
 )
import WebGear.Server.Handler (ServerHandler)

instance (Monad m) => Get (ServerHandler m) (BasicAuth' Required scheme m e a) where
  {-# INLINE getTrait #-}
  getTrait ::
    (HasTrait (AuthorizationHeader scheme) ts) =>
    BasicAuth' Required scheme m e a ->
    ServerHandler m (Request `With` ts) (Either (BasicAuthError e) a)
  getTrait BasicAuth'{..} = proc request -> do
    let result = pick @(AuthorizationHeader scheme) $ from request
    case result of
      Nothing -> returnA -< Left BasicAuthHeaderMissing
      (Just (Left _)) -> returnA -< Left BasicAuthSchemeMismatch
      (Just (Right token)) ->
        case parseCreds token of
          Left e -> returnA -< Left e
          Right c -> validateCreds -< c
    where
      parseCreds :: AuthToken scheme -> Either (BasicAuthError e) Credentials
      parseCreds AuthToken{..} =
        case split ':' (decodeLenient authToken) of
          [] -> Left BasicAuthCredsBadFormat
          u : ps -> Right $ Credentials (Username u) (Password $ intercalate ":" ps)

      validateCreds :: ServerHandler m Credentials (Either (BasicAuthError e) a)
      validateCreds = arrM $ \creds -> do
        res <- toBasicAttribute creds
        pure $ first BasicAuthAttributeError res

instance (Monad m) => Get (ServerHandler m) (BasicAuth' Optional scheme m e a) where
  {-# INLINE getTrait #-}
  getTrait ::
    (HasTrait (AuthorizationHeader scheme) ts) =>
    BasicAuth' Optional scheme m e a ->
    ServerHandler m (Request `With` ts) (Either Void (Either (BasicAuthError e) a))
  getTrait BasicAuth'{..} = getTrait (BasicAuth'{..} :: BasicAuth' Required scheme m e a) >>> arr Right
