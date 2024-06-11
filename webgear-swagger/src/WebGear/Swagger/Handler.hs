{-# LANGUAGE CPP #-}

{- | An implementation of `Handler` to generate `Swagger` documentation
 from WebGear API specifications.
-}
module WebGear.Swagger.Handler (
  SwaggerHandler (..),
  Documentation (..),
  consumeDescription,
  consumeSummary,
  addRouteDocumentation,
  addRootPath,
  toSwagger,
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
import Data.Swagger (
  Operation,
  PathItem,
  Referenced (..),
  Response,
  Swagger,
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
  summary,
 )
import WebGear.Core.Handler (
  Description (..),
  Handler (..),
  RouteMismatch,
  RoutePath (..),
  Summary (..),
 )

-- | A handler that captures `Swagger` documentation of API specifications.
newtype SwaggerHandler m a b = SwaggerHandler (Swagger -> State Documentation Swagger)

data Documentation = Documentation !(Maybe Description) !(Maybe Summary)

consumeDescription :: (MonadState Documentation m) => m (Maybe Description)
consumeDescription = state $ \(Documentation d s) -> (d, Documentation Nothing s)

consumeSummary :: (MonadState Documentation m) => m (Maybe Summary)
consumeSummary = state $ \(Documentation d s) -> (s, Documentation d Nothing)

addRouteDocumentation :: (MonadState Documentation m) => Swagger -> m Swagger
addRouteDocumentation doc = do
  desc <- consumeDescription
  summ <- consumeSummary
  pure $
    doc
      -- keep any existing documentation
      & allOperations . summary %~ (<|> fmap getSummary summ)
      & allOperations . description %~ (<|> fmap getDescription desc)

addRootPath :: Swagger -> Swagger
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

    opr :: Operation
    opr = mempty @Operation & at 0 ?~ Inline (mempty @Response)

instance Cat.Category (SwaggerHandler m) where
  {-# INLINE id #-}
  id :: SwaggerHandler m a a
  id = SwaggerHandler pure

  {-# INLINE (.) #-}
  (.) :: SwaggerHandler m b c -> SwaggerHandler m a b -> SwaggerHandler m a c
  SwaggerHandler g . SwaggerHandler f = SwaggerHandler $ f <=< g

instance Arrow (SwaggerHandler m) where
  {-# INLINE arr #-}
  arr :: (a -> b) -> SwaggerHandler m a b
  arr _ = SwaggerHandler pure

  {-# INLINE first #-}
  first :: SwaggerHandler m b c -> SwaggerHandler m (b, d) (c, d)
  first = coerce

  {-# INLINE second #-}
  second :: SwaggerHandler m b c -> SwaggerHandler m (d, b) (d, c)
  second = coerce

instance ArrowZero (SwaggerHandler m) where
  {-# INLINE zeroArrow #-}
  zeroArrow :: SwaggerHandler m b c
  zeroArrow = SwaggerHandler pure

newtype MergeSwagger = MergeSwagger (Swagger -> State Documentation Swagger)

instance Semigroup MergeSwagger where
  MergeSwagger f <> MergeSwagger g =
    MergeSwagger $ \doc -> do
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
          & parameters .~ (x ^. parameters <> y ^. parameters)

instance ArrowPlus (SwaggerHandler m) where
  {-# INLINE (<+>) #-}
  (<+>) :: SwaggerHandler m b c -> SwaggerHandler m b c -> SwaggerHandler m b c
  SwaggerHandler f <+> SwaggerHandler g = coerce $ MergeSwagger f <> MergeSwagger g

instance ArrowChoice (SwaggerHandler m) where
  {-# INLINE left #-}
  left :: SwaggerHandler m b c -> SwaggerHandler m (Either b d) (Either c d)
  left (SwaggerHandler doc) = SwaggerHandler doc

  {-# INLINE right #-}
  right :: SwaggerHandler m b c -> SwaggerHandler m (Either d b) (Either d c)
  right (SwaggerHandler doc) = SwaggerHandler doc

  {-# INLINE (+++) #-}
  (+++) :: SwaggerHandler m b c -> SwaggerHandler m b' c' -> SwaggerHandler m (Either b b') (Either c c')
  SwaggerHandler f +++ SwaggerHandler g = coerce $ MergeSwagger f <> MergeSwagger g

  {-# INLINE (|||) #-}
  (|||) :: SwaggerHandler m b d -> SwaggerHandler m c d -> SwaggerHandler m (Either b c) d
  SwaggerHandler f ||| SwaggerHandler g = coerce $ MergeSwagger f <> MergeSwagger g

instance ArrowError RouteMismatch (SwaggerHandler m) where
  {-# INLINE raise #-}
  raise = SwaggerHandler pure

  {-# INLINE handle #-}
  SwaggerHandler f `handle` SwaggerHandler g = coerce $ MergeSwagger f <> MergeSwagger g

  {-# INLINE tryInUnless #-}
  tryInUnless (SwaggerHandler f) (SwaggerHandler g) (SwaggerHandler h) =
    coerce $ MergeSwagger f <> MergeSwagger g <> MergeSwagger h

instance (Monad m) => Handler (SwaggerHandler m) m where
  {-# INLINE arrM #-}
  arrM :: (a -> m b) -> SwaggerHandler m a b
  arrM _ = SwaggerHandler pure

  {-# INLINE consumeRoute #-}
  consumeRoute :: SwaggerHandler m RoutePath a -> SwaggerHandler m () a
  consumeRoute (SwaggerHandler f) = SwaggerHandler f

  {-# INLINE setDescription #-}
  setDescription :: Description -> SwaggerHandler m a a
  setDescription d = SwaggerHandler $ \doc ->
    state $ \(Documentation _ s) -> (doc, Documentation (Just d) s)

  {-# INLINE setSummary #-}
  setSummary :: Summary -> SwaggerHandler m a a
  setSummary s = SwaggerHandler $ \doc ->
    state $ \(Documentation d _) -> (doc, Documentation d (Just s))

-- | Generate Swagger documentation from a handler
toSwagger :: SwaggerHandler m a b -> Swagger
toSwagger (SwaggerHandler f) = evalState (f mempty) (Documentation Nothing Nothing)
