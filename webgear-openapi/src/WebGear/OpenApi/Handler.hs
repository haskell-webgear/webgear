{-# LANGUAGE CPP #-}

{- | An implementation of `Handler` to generate `OpenApi` documentation
 from WebGear API specifications.
-}
module WebGear.OpenApi.Handler (
  OpenApiHandler (..),
  Documentation (..),
  consumeDescription,
  consumeSummary,
  addRouteDocumentation,
  addRootPath,
  toOpenApi,
) where

import Control.Applicative ((<|>))
import Control.Arrow (Arrow (..), ArrowChoice (..), ArrowPlus (..), ArrowZero (..))
import Control.Arrow.Operations (ArrowError (..))
import qualified Control.Category as Cat
import Control.Lens (at, (%~), (&), (.~), (?~), (^.))
import Control.Monad ((<=<))
import Control.Monad.State.Strict (MonadState, State, evalState, state)
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict.InsOrd as Map
import Data.OpenApi (
  OpenApi,
  Operation,
  PathItem,
  Referenced (..),
  Response,
  allOperations,
  delete,
  description,
  externalDocs,
  get,
  head_,
  options,
  parameters,
  patch,
  paths,
  post,
  put,
  servers,
  summary,
  trace,
 )
import WebGear.Core.Handler (
  Description (..),
  Handler (..),
  RouteMismatch,
  RoutePath (..),
  Summary (..),
 )

-- | A handler that captures `OpenApi` documentation of API specifications.
newtype OpenApiHandler m a b = OpenApiHandler (OpenApi -> State Documentation OpenApi)

data Documentation = Documentation !(Maybe Description) !(Maybe Summary)

consumeDescription :: (MonadState Documentation m) => m (Maybe Description)
consumeDescription = state $ \(Documentation d s) -> (d, Documentation Nothing s)

consumeSummary :: (MonadState Documentation m) => m (Maybe Summary)
consumeSummary = state $ \(Documentation d s) -> (s, Documentation d Nothing)

addRouteDocumentation :: (MonadState Documentation m) => OpenApi -> m OpenApi
addRouteDocumentation doc = do
  desc <- consumeDescription
  summ <- consumeSummary
  pure $
    doc
      -- keep any existing documentation
      & allOperations . summary %~ (<|> fmap getSummary summ)
      & allOperations . description %~ (<|> fmap getDescription desc)

addRootPath :: OpenApi -> OpenApi
addRootPath doc = doc & paths .~ [("/", rootPathItem)]
  where
    rootPathItem :: PathItem
    rootPathItem =
      mempty @PathItem
        & delete ?~ opr
        & get ?~ opr
        & head_ ?~ opr
        & options ?~ opr
        & patch ?~ opr
        & post ?~ opr
        & put ?~ opr
        & trace ?~ opr

    opr :: Operation
    opr = mempty @Operation & at 0 ?~ Inline (mempty @Response)

instance Cat.Category (OpenApiHandler m) where
  {-# INLINE id #-}
  id :: OpenApiHandler m a a
  id = OpenApiHandler pure

  {-# INLINE (.) #-}
  (.) :: OpenApiHandler m b c -> OpenApiHandler m a b -> OpenApiHandler m a c
  OpenApiHandler g . OpenApiHandler f = OpenApiHandler $ f <=< g

instance Arrow (OpenApiHandler m) where
  {-# INLINE arr #-}
  arr :: (a -> b) -> OpenApiHandler m a b
  arr _ = OpenApiHandler pure

  {-# INLINE first #-}
  first :: OpenApiHandler m b c -> OpenApiHandler m (b, d) (c, d)
  first = coerce

  {-# INLINE second #-}
  second :: OpenApiHandler m b c -> OpenApiHandler m (d, b) (d, c)
  second = coerce

instance ArrowZero (OpenApiHandler m) where
  {-# INLINE zeroArrow #-}
  zeroArrow :: OpenApiHandler m b c
  zeroArrow = OpenApiHandler pure

newtype MergeOpenApi = MergeOpenApi (OpenApi -> State Documentation OpenApi)

instance Semigroup MergeOpenApi where
  MergeOpenApi f <> MergeOpenApi g =
    MergeOpenApi $ \doc -> do
      a <- f doc
      b <- g doc
      pure $
        (a <> b)
          & paths .~ Map.unionWith mergePathItem (a ^. paths) (b ^. paths)
          & externalDocs .~ (a ^. externalDocs <|> b ^. externalDocs)
    where
      mergePathItem :: PathItem -> PathItem -> PathItem
      mergePathItem x y =
        mempty @PathItem
          & delete .~ x ^. delete <> y ^. delete
          & get .~ x ^. get <> y ^. get
          & head_ .~ x ^. head_ <> y ^. head_
          & options .~ x ^. options <> y ^. options
          & patch .~ x ^. patch <> y ^. patch
          & post .~ x ^. post <> y ^. post
          & put .~ x ^. put <> y ^. put
          & trace .~ x ^. trace <> y ^. trace
          & summary .~ (x ^. summary <|> y ^. summary)
          & description .~ (x ^. description <|> y ^. description)
          & parameters .~ (x ^. parameters <> y ^. parameters)
          & servers .~ (x ^. servers <> y ^. servers)

instance ArrowPlus (OpenApiHandler m) where
  {-# INLINE (<+>) #-}
  (<+>) :: OpenApiHandler m b c -> OpenApiHandler m b c -> OpenApiHandler m b c
  OpenApiHandler f <+> OpenApiHandler g = coerce $ MergeOpenApi f <> MergeOpenApi g

instance ArrowChoice (OpenApiHandler m) where
  {-# INLINE left #-}
  left :: OpenApiHandler m b c -> OpenApiHandler m (Either b d) (Either c d)
  left (OpenApiHandler doc) = OpenApiHandler doc

  {-# INLINE right #-}
  right :: OpenApiHandler m b c -> OpenApiHandler m (Either d b) (Either d c)
  right (OpenApiHandler doc) = OpenApiHandler doc

  {-# INLINE (+++) #-}
  (+++) :: OpenApiHandler m b c -> OpenApiHandler m b' c' -> OpenApiHandler m (Either b b') (Either c c')
  OpenApiHandler f +++ OpenApiHandler g = coerce $ MergeOpenApi f <> MergeOpenApi g

  {-# INLINE (|||) #-}
  (|||) :: OpenApiHandler m b d -> OpenApiHandler m c d -> OpenApiHandler m (Either b c) d
  OpenApiHandler f ||| OpenApiHandler g = coerce $ MergeOpenApi f <> MergeOpenApi g

instance ArrowError RouteMismatch (OpenApiHandler m) where
  {-# INLINE raise #-}
  raise = OpenApiHandler pure

  {-# INLINE handle #-}
  OpenApiHandler f `handle` OpenApiHandler g = coerce $ MergeOpenApi f <> MergeOpenApi g

  {-# INLINE tryInUnless #-}
  tryInUnless (OpenApiHandler f) (OpenApiHandler g) (OpenApiHandler h) =
    coerce $ MergeOpenApi f <> MergeOpenApi g <> MergeOpenApi h

instance (Monad m) => Handler (OpenApiHandler m) m where
  {-# INLINE arrM #-}
  arrM :: (a -> m b) -> OpenApiHandler m a b
  arrM _ = OpenApiHandler pure

  {-# INLINE consumeRoute #-}
  consumeRoute :: OpenApiHandler m RoutePath a -> OpenApiHandler m () a
  consumeRoute (OpenApiHandler f) = OpenApiHandler f

  {-# INLINE setDescription #-}
  setDescription :: Description -> OpenApiHandler m a a
  setDescription d = OpenApiHandler $ \doc ->
    state $ \(Documentation _ s) -> (doc, Documentation (Just d) s)

  {-# INLINE setSummary #-}
  setSummary :: Summary -> OpenApiHandler m a a
  setSummary s = OpenApiHandler $ \doc ->
    state $ \(Documentation d _) -> (doc, Documentation d (Just s))

-- | Generate OpenApi documentation from a handler
toOpenApi :: OpenApiHandler m a b -> OpenApi
toOpenApi (OpenApiHandler f) = evalState (f mempty) (Documentation Nothing Nothing)
