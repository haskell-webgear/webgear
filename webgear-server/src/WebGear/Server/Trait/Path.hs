{-# OPTIONS_GHC -Wno-orphans #-}

-- | Server implementation of the path traits.
module WebGear.Server.Trait.Path where

import Control.Monad.State (get, gets, put)
import qualified Data.List as List
import qualified Data.Text as Text
import Web.HttpApiData (FromHttpApiData (..))
import WebGear.Core.Handler (RoutePath (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..), With)
import WebGear.Core.Trait.Path (
  Path (..),
  PathEnd (..),
  PathVar (..),
  PathVarError (..),
 )
import WebGear.Server.Handler (ServerHandler (..))

instance (Monad m) => Get (ServerHandler m) Path where
  {-# INLINE getTrait #-}
  getTrait :: Path -> ServerHandler m (Request `With` ts) (Either () ())
  getTrait (Path p) = ServerHandler $ const $ do
    RoutePath remaining <- get
    let expected = filter (/= "") $ Text.splitOn "/" p
    case List.stripPrefix expected remaining of
      Just ps -> put (RoutePath ps) >> pure (Right ())
      Nothing -> pure (Left ())

instance (Monad m, FromHttpApiData val) => Get (ServerHandler m) (PathVar tag val) where
  {-# INLINE getTrait #-}
  getTrait :: PathVar tag val -> ServerHandler m (Request `With` ts) (Either PathVarError val)
  getTrait PathVar = ServerHandler $ const $ do
    RoutePath remaining <- get
    case remaining of
      [] -> pure (Left PathVarNotFound)
      (p : ps) ->
        case parseUrlPiece p of
          Left e -> pure (Left $ PathVarParseError e)
          Right val -> put (RoutePath ps) >> pure (Right val)

instance (Monad m) => Get (ServerHandler m) PathEnd where
  {-# INLINE getTrait #-}
  getTrait :: PathEnd -> ServerHandler m (Request `With` ts) (Either () ())
  getTrait PathEnd =
    ServerHandler $
      const $
        gets
          ( \case
              RoutePath [] -> Right ()
              _ -> Left ()
          )
