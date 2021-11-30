{-# OPTIONS_GHC -Wno-orphans #-}

module WebGear.Server.Middleware.Path where

import qualified Data.List as List
import qualified Data.Text as Text
import Web.HttpApiData (FromHttpApiData (..))
import WebGear.Core.Handler (RoutePath (..))
import WebGear.Core.Middleware.Path (Path (..), PathEnd (..), PathVar (..), PathVarError (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..), Linked)
import WebGear.Server.Handler (ServerHandler (..))

instance Monad m => Get (ServerHandler m) Path Request where
  {-# INLINEABLE getTrait #-}
  getTrait :: Path -> ServerHandler m (Linked ts Request) (Either () ())
  getTrait (Path p) = ServerHandler $ \(_, path@(RoutePath remaining)) -> do
    let expected = filter (/= "") $ Text.splitOn "/" p
    pure $ case List.stripPrefix expected remaining of
      Just ps -> (Right (Right ()), RoutePath ps)
      Nothing -> (Right (Left ()), path)

instance (Monad m, FromHttpApiData val) => Get (ServerHandler m) (PathVar tag val) Request where
  {-# INLINEABLE getTrait #-}
  getTrait :: PathVar tag val -> ServerHandler m (Linked ts Request) (Either PathVarError val)
  getTrait PathVar = ServerHandler $ \(_, path@(RoutePath remaining)) -> do
    pure $ case remaining of
      [] -> (Right (Left PathVarNotFound), path)
      (p : ps) ->
        case parseUrlPiece p of
          Left e -> (Right (Left $ PathVarParseError e), path)
          Right val -> (Right (Right val), RoutePath ps)

instance Monad m => Get (ServerHandler m) PathEnd Request where
  {-# INLINEABLE getTrait #-}
  getTrait :: PathEnd -> ServerHandler m (Linked ts Request) (Either () ())
  getTrait PathEnd = ServerHandler f
    where
      f (_, p@(RoutePath [])) = pure (Right $ Right (), p)
      f (_, p) = pure (Right $ Left (), p)
