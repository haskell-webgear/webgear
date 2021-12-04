{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WebGear.Server.Middleware.Auth.JWT where

import Control.Arrow (arr, returnA, (>>>))
import Control.Monad.Except (MonadError (throwError), lift, runExceptT, withExceptT)
import Control.Monad.Time (MonadTime)
import qualified Crypto.JWT as JWT
import Data.ByteString.Lazy (fromStrict)
import Data.Void (Void)
import WebGear.Core.Handler (arrM)
import WebGear.Core.Middleware.Auth.Common (
  AuthToken (..),
  AuthorizationHeader,
  getAuthorizationHeaderTrait,
 )
import WebGear.Core.Middleware.Auth.JWT (JWTAuth' (..), JWTAuthError (..))
import WebGear.Core.Modifiers
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..), Linked)
import WebGear.Server.Handler (ServerHandler)

parseJWT :: AuthToken scheme -> Either JWT.JWTError JWT.SignedJWT
parseJWT AuthToken{..} = JWT.decodeCompact $ fromStrict authToken

instance (MonadTime m, Get (ServerHandler m) (AuthorizationHeader scheme) Request) => Get (ServerHandler m) (JWTAuth' Required scheme m e a) Request where
  {-# INLINEABLE getTrait #-}
  getTrait ::
    JWTAuth' Required scheme m e a ->
    ServerHandler m (Linked ts Request) (Either (JWTAuthError e) a)
  getTrait JWTAuth'{..} = proc request -> do
    result <- getAuthorizationHeaderTrait @scheme -< request
    case result of
      Nothing -> returnA -< Left JWTAuthHeaderMissing
      (Just (Left _)) -> returnA -< Left JWTAuthSchemeMismatch
      (Just (Right token)) ->
        case parseJWT token of
          Left e -> returnA -< Left (JWTAuthTokenBadFormat e)
          Right jwt -> validateJWT -< jwt
    where
      validateJWT :: ServerHandler m JWT.SignedJWT (Either (JWTAuthError e) a)
      validateJWT = arrM $ \jwt -> runExceptT $ do
        claims <- withExceptT JWTAuthTokenBadFormat $ JWT.verifyClaims jwtValidationSettings jwkSet jwt
        lift (toJWTAttribute claims) >>= either (throwError . JWTAuthAttributeError) pure

instance (MonadTime m, Get (ServerHandler m) (AuthorizationHeader scheme) Request) => Get (ServerHandler m) (JWTAuth' Optional scheme m e a) Request where
  {-# INLINEABLE getTrait #-}
  getTrait ::
    JWTAuth' Optional scheme m e a ->
    ServerHandler m (Linked ts Request) (Either Void (Either (JWTAuthError e) a))
  getTrait JWTAuth'{..} = getTrait (JWTAuth'{..} :: JWTAuth' Required scheme m e a) >>> arr Right
