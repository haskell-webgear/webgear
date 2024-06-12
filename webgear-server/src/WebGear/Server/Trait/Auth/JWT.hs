{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Server implementation of the 'JWTAuth'' trait.
module WebGear.Server.Trait.Auth.JWT where

import Control.Arrow (arr, returnA, (>>>))
import Control.Monad.Except (MonadError (throwError), runExceptT, withExceptT)
import Control.Monad.Time (MonadTime)
import Control.Monad.Trans (lift)
import qualified Crypto.JWT as JWT
import Data.ByteString.Lazy (fromStrict)
import Data.Void (Void)
import WebGear.Core.Handler (arrM)
import WebGear.Core.Modifiers
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..), HasTrait, With, from, pick)
import WebGear.Core.Trait.Auth.Common (
  AuthToken (..),
  AuthorizationHeader,
 )
import WebGear.Core.Trait.Auth.JWT (JWTAuth' (..), JWTAuthError (..))
import WebGear.Server.Handler (ServerHandler)

instance (MonadTime m, Get (ServerHandler m) (AuthorizationHeader scheme)) => Get (ServerHandler m) (JWTAuth' Required scheme m e a) where
  {-# INLINE getTrait #-}
  getTrait ::
    (HasTrait (AuthorizationHeader scheme) ts) =>
    JWTAuth' Required scheme m e a ->
    ServerHandler m (Request `With` ts) (Either (JWTAuthError e) a)
  getTrait JWTAuth'{..} = proc request -> do
    let result = pick @(AuthorizationHeader scheme) $ from request
    case result of
      Nothing -> returnA -< Left JWTAuthHeaderMissing
      (Just (Left _)) -> returnA -< Left JWTAuthSchemeMismatch
      (Just (Right token)) ->
        case parseJWT token of
          Left e -> returnA -< Left (JWTAuthTokenBadFormat e)
          Right jwt -> validateJWT -< jwt
    where
      parseJWT :: AuthToken scheme -> Either JWT.JWTError JWT.SignedJWT
      parseJWT AuthToken{..} = JWT.decodeCompact $ fromStrict authToken

      validateJWT :: ServerHandler m JWT.SignedJWT (Either (JWTAuthError e) a)
      validateJWT = arrM $ \jwt -> runExceptT $ do
        claims <- withExceptT JWTAuthTokenBadFormat $ JWT.verifyClaims jwtValidationSettings jwkSet jwt
        lift (toJWTAttribute claims) >>= either (throwError . JWTAuthAttributeError) pure

instance (MonadTime m, Get (ServerHandler m) (AuthorizationHeader scheme)) => Get (ServerHandler m) (JWTAuth' Optional scheme m e a) where
  {-# INLINE getTrait #-}
  getTrait ::
    (HasTrait (AuthorizationHeader scheme) ts) =>
    JWTAuth' Optional scheme m e a ->
    ServerHandler m (Request `With` ts) (Either Void (Either (JWTAuthError e) a))
  getTrait JWTAuth'{..} = getTrait (JWTAuth'{..} :: JWTAuth' Required scheme m e a) >>> arr Right
