{- | An implementation of `Handler` to generate `Swagger` documentation
 from WebGear API specifications.
-}
module WebGear.Swagger.Handler (
  SwaggerHandler (..),
  DocNode (..),
  Tree (..),
  singletonNode,
  nullNode,
  toSwagger,
) where

import Control.Applicative ((<|>))
import Control.Arrow (Arrow (..), ArrowChoice (..), ArrowPlus (..), ArrowZero (..))
import Control.Arrow.Operations (ArrowError (..))
import qualified Control.Category as Cat
import Control.Lens (at, (%~), (&), (.~), (<>~), (?~), (^.))
import qualified Data.HashMap.Strict.InsOrd as Map
import Data.List (nub)
import Data.Swagger
import Data.Swagger.Internal.Utils (swaggerMappend)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP
import WebGear.Core.Handler (Description (..), Handler (..), RouteMismatch, RoutePath (..), Summary (..))

-- | A tree where internal nodes have one or two children.
data Tree a
  = NullNode
  | SingleNode a (Tree a)
  | BinaryNode (Tree a) (Tree a)
  deriving stock (Show)

instance Semigroup (Tree a) where
  (<>) :: Tree a -> Tree a -> Tree a
  parent <> child =
    case parent of
      NullNode -> child
      SingleNode doc next -> SingleNode doc (next <> child)
      BinaryNode b1 b2 -> BinaryNode (b1 <> child) (b2 <> child)

instance Monoid (Tree a) where
  mempty = NullNode
  mappend = (<>)

-- | Different types of documentation elements captured by the handler
data DocNode
  = DocSecurityScheme Text SecurityScheme
  | DocRequestBody (Definitions Schema) MimeList Param
  | DocResponseBody (Definitions Schema) MimeList (Maybe (Referenced Schema))
  | DocRequestHeader Param
  | DocResponseHeader HeaderName Header
  | DocMethod HTTP.StdMethod
  | DocPathElem Text
  | DocPathVar Param
  | DocQueryParam Param
  | DocStatus HTTP.Status
  | DocSummary Summary
  | DocDescription Description
  deriving stock (Show)

-- | Documentation elements after compaction
data CompactDocNode
  = CDocSecurityScheme Text SecurityScheme
  | CDocRequestBody (Definitions Schema) MimeList Param
  | CDocResponseBody (Definitions Schema) MimeList (Maybe (Referenced Schema))
  | CDocRequestHeader Param
  | CDocResponseHeader HeaderName Header
  | CDocMethod HTTP.StdMethod
  | CDocPathElem Text
  | CDocPathVar Param
  | CDocRouteDoc (Maybe Summary) (Maybe Description)
  | CDocQueryParam Param
  | CDocStatus HTTP.Status (Maybe Description)
  deriving stock (Show)

-- | Generate a tree with a single node
singletonNode :: a -> Tree a
singletonNode a = SingleNode a NullNode

-- | Generate an empty tree
nullNode :: Tree a
nullNode = NullNode

{- | A handler that captured `Swagger` documentation of API
 specifications.
-}
newtype SwaggerHandler m a b = SwaggerHandler
  {swaggerDoc :: Tree DocNode}

instance Cat.Category (SwaggerHandler m) where
  id :: SwaggerHandler m a a
  id = SwaggerHandler{swaggerDoc = NullNode}

  (.) :: SwaggerHandler m b c -> SwaggerHandler m a b -> SwaggerHandler m a c
  SwaggerHandler doc2 . SwaggerHandler doc1 = SwaggerHandler $ insertAsLeaf doc1 doc2
    where
      insertAsLeaf :: Tree DocNode -> Tree DocNode -> Tree DocNode
      insertAsLeaf parent child = case parent of
        NullNode -> child
        SingleNode doc next -> SingleNode doc (insertAsLeaf next child)
        BinaryNode b1 b2 -> BinaryNode (insertAsLeaf b1 child) (insertAsLeaf b2 child)

instance Arrow (SwaggerHandler m) where
  arr :: (a -> b) -> SwaggerHandler m a b
  arr _ = SwaggerHandler{swaggerDoc = NullNode}

  first :: SwaggerHandler m b c -> SwaggerHandler m (b, d) (c, d)
  first (SwaggerHandler doc) = SwaggerHandler doc

  second :: SwaggerHandler m b c -> SwaggerHandler m (d, b) (d, c)
  second (SwaggerHandler doc) = SwaggerHandler doc

instance ArrowZero (SwaggerHandler m) where
  zeroArrow :: SwaggerHandler m b c
  zeroArrow = SwaggerHandler{swaggerDoc = NullNode}

instance ArrowPlus (SwaggerHandler m) where
  (<+>) :: SwaggerHandler m b c -> SwaggerHandler m b c -> SwaggerHandler m b c
  SwaggerHandler NullNode <+> SwaggerHandler doc = SwaggerHandler doc
  SwaggerHandler doc <+> SwaggerHandler NullNode = SwaggerHandler doc
  SwaggerHandler doc1 <+> SwaggerHandler doc2 = SwaggerHandler $ BinaryNode doc1 doc2

instance ArrowChoice (SwaggerHandler m) where
  left :: SwaggerHandler m b c -> SwaggerHandler m (Either b d) (Either c d)
  left (SwaggerHandler doc) = SwaggerHandler doc

  right :: SwaggerHandler m b c -> SwaggerHandler m (Either d b) (Either d c)
  right (SwaggerHandler doc) = SwaggerHandler doc

  (+++) :: SwaggerHandler m b c -> SwaggerHandler m b' c' -> SwaggerHandler m (Either b b') (Either c c')
  SwaggerHandler doc +++ SwaggerHandler NullNode = SwaggerHandler doc
  SwaggerHandler NullNode +++ SwaggerHandler doc = SwaggerHandler doc
  SwaggerHandler doc1 +++ SwaggerHandler doc2 = SwaggerHandler $ BinaryNode doc1 doc2

  (|||) :: SwaggerHandler m b d -> SwaggerHandler m c d -> SwaggerHandler m (Either b c) d
  SwaggerHandler doc ||| SwaggerHandler NullNode = SwaggerHandler doc
  SwaggerHandler NullNode ||| SwaggerHandler doc = SwaggerHandler doc
  SwaggerHandler doc1 ||| SwaggerHandler doc2 = SwaggerHandler $ BinaryNode doc1 doc2

instance ArrowError RouteMismatch (SwaggerHandler m) where
  {-# INLINE raise #-}
  raise = SwaggerHandler{swaggerDoc = NullNode}

  {-# INLINE handle #-}
  SwaggerHandler doc1 `handle` SwaggerHandler doc2 = SwaggerHandler $ BinaryNode doc1 doc2

  {-# INLINE tryInUnless #-}
  tryInUnless (SwaggerHandler doc1) (SwaggerHandler doc2) (SwaggerHandler doc3) =
    SwaggerHandler $ BinaryNode (BinaryNode doc1 doc2) doc3

instance (Monad m) => Handler (SwaggerHandler m) m where
  {-# INLINE arrM #-}
  arrM :: (a -> m b) -> SwaggerHandler m a b
  arrM _ = SwaggerHandler{swaggerDoc = NullNode}

  {-# INLINE consumeRoute #-}
  consumeRoute :: SwaggerHandler m RoutePath a -> SwaggerHandler m () a
  consumeRoute (SwaggerHandler doc) = SwaggerHandler doc

  {-# INLINE setDescription #-}
  setDescription :: Description -> SwaggerHandler m a a
  setDescription = SwaggerHandler . singletonNode . DocDescription

  {-# INLINE setSummary #-}
  setSummary :: Summary -> SwaggerHandler m a a
  setSummary = SwaggerHandler . singletonNode . DocSummary

-- | Generate Swagger documentation from a handler
toSwagger :: SwaggerHandler m a b -> Swagger
toSwagger = go . compact . swaggerDoc
  where
    go t = case t of
      NullNode -> mempty
      SingleNode parent child -> mergeDoc parent child mempty
      BinaryNode t1 t2 -> go t1 `combineSwagger` go t2

compact :: Tree DocNode -> Tree CompactDocNode
compact t = let (_, _, t') = go t in t'
  where
    go = \case
      NullNode -> (Nothing, Nothing, NullNode)
      BinaryNode t1 t2 ->
        let (descr1, summ1, t1') = go t1
            (descr2, summ2, t2') = go t2
         in (descr1 <|> descr2, summ1 <|> summ2, BinaryNode t1' t2')
      SingleNode node child -> compactDoc node child

    compactDoc :: DocNode -> Tree DocNode -> (Maybe Description, Maybe Summary, Tree CompactDocNode)
    compactDoc (DocSecurityScheme schemeName scheme) child =
      let (descr, summ, child') = go child
          scheme' = scheme & description .~ fmap getDescription descr
       in (Nothing, summ, SingleNode (CDocSecurityScheme schemeName scheme') child')
    compactDoc (DocRequestBody defs mimeList bodyParam) child =
      let (descr, summ, child') = go child
          bodyParam' = bodyParam & description .~ fmap getDescription descr
       in (Nothing, summ, SingleNode (CDocRequestBody defs mimeList bodyParam') child')
    compactDoc (DocResponseBody defs mimeList responseSchema) child =
      SingleNode (CDocResponseBody defs mimeList responseSchema) <$> go child
    compactDoc (DocRequestHeader param) child =
      let (descr, summ, child') = go child
          param' = param & description .~ fmap getDescription descr
       in (Nothing, summ, SingleNode (CDocRequestHeader param') child')
    compactDoc (DocResponseHeader headerName header) child =
      let (descr, summ, child') = go child
          header' = header & description .~ fmap getDescription descr
       in (Nothing, summ, SingleNode (CDocResponseHeader headerName header') child')
    compactDoc (DocMethod m) child =
      (Nothing, Nothing, addRouteDoc (CDocMethod m) child)
    compactDoc (DocPathElem path) child =
      (Nothing, Nothing, addRouteDoc (CDocPathElem path) child)
    compactDoc (DocPathVar param) child =
      (Nothing, Nothing, addRouteDoc (CDocPathVar param) child)
    compactDoc (DocQueryParam param) child =
      let (descr, summ, child') = go child
          param' = param & description .~ fmap getDescription descr
       in (Nothing, summ, SingleNode (CDocQueryParam param') child')
    compactDoc (DocStatus status) child =
      let (descr, summ, child') = go child
       in (Nothing, summ, SingleNode (CDocStatus status descr) child')
    compactDoc (DocSummary summ) child =
      let (descr, _, child') = go child
       in (descr, Just summ, child')
    compactDoc (DocDescription descr) child =
      let (_, summ, child') = go child
       in (Just descr, summ, child')

    addRouteDoc :: CompactDocNode -> Tree DocNode -> Tree CompactDocNode
    addRouteDoc node child = case go child of
      (Nothing, Nothing, child') -> SingleNode node child'
      (descr, summ, child') -> SingleNode (CDocRouteDoc summ descr) (SingleNode node child')

postOrder :: Tree CompactDocNode -> Swagger -> (Swagger -> Swagger) -> Swagger
postOrder NullNode doc f = f doc
postOrder (SingleNode node child) doc f = f $ mergeDoc node child doc
postOrder (BinaryNode t1 t2) doc f =
  f $ postOrder t1 doc id `combineSwagger` postOrder t2 doc id

preOrder :: Tree CompactDocNode -> Swagger -> (Swagger -> Swagger) -> Swagger
preOrder NullNode doc f = f doc
preOrder (SingleNode node child) doc f = mergeDoc node child (f doc)
preOrder (BinaryNode t1 t2) doc f =
  let doc' = f doc
   in postOrder t1 doc' id `combineSwagger` postOrder t2 doc' id

newtype WHOperation = WHOperation {getOperation :: Operation}

instance Semigroup WHOperation where
  WHOperation op1 <> WHOperation op2 =
    let nubMimeList = fmap (\(MimeList xs) -> MimeList $ nub xs)
     in WHOperation $ (op1 <> op2)
          & consumes .~ nubMimeList ((op1 ^. consumes) <> (op2 ^. consumes))
          & produces .~ nubMimeList ((op1 ^. produces) <> (op2 ^. produces))

combineOperation :: Maybe Operation -> Maybe Operation -> Maybe Operation
combineOperation op1 op2 = getOperation <$> (WHOperation <$> op1) <> (WHOperation <$> op2)

combinePathItem :: PathItem -> PathItem -> PathItem
combinePathItem s t =
  PathItem
    { _pathItemGet = _pathItemGet s `combineOperation` _pathItemGet t
    , _pathItemPut = _pathItemPut s `combineOperation` _pathItemPut t
    , _pathItemPost = _pathItemPost s `combineOperation` _pathItemPost t
    , _pathItemDelete = _pathItemDelete s `combineOperation` _pathItemDelete t
    , _pathItemOptions = _pathItemOptions s `combineOperation` _pathItemOptions t
    , _pathItemHead = _pathItemHead s `combineOperation` _pathItemHead t
    , _pathItemPatch = _pathItemPatch s `combineOperation` _pathItemPatch t
    , _pathItemParameters = _pathItemParameters s <> _pathItemParameters t
    }

combineSwagger :: Swagger -> Swagger -> Swagger
combineSwagger s t =
  Swagger
    { _swaggerInfo = _swaggerInfo s <> _swaggerInfo t
    , _swaggerHost = _swaggerHost s <|> _swaggerHost t
    , _swaggerBasePath = _swaggerBasePath s <> _swaggerBasePath t
    , _swaggerSchemes = _swaggerSchemes s <> _swaggerSchemes t
    , _swaggerConsumes = _swaggerConsumes s <> _swaggerConsumes t
    , _swaggerProduces = _swaggerProduces s <> _swaggerProduces t
    , _swaggerDefinitions = _swaggerDefinitions s <> _swaggerDefinitions t
    , _swaggerParameters = _swaggerParameters s <> _swaggerParameters t
    , _swaggerResponses = _swaggerResponses s <> _swaggerResponses t
    , _swaggerSecurityDefinitions = _swaggerSecurityDefinitions s <> _swaggerSecurityDefinitions t
    , _swaggerPaths = Map.unionWith combinePathItem (_swaggerPaths s) (_swaggerPaths t)
    , _swaggerSecurity = _swaggerSecurity s <> _swaggerSecurity t
    , _swaggerTags = _swaggerTags s <> _swaggerTags t
    , _swaggerExternalDocs = _swaggerExternalDocs s <|> _swaggerExternalDocs t
    }

mergeDoc :: CompactDocNode -> Tree CompactDocNode -> Swagger -> Swagger
mergeDoc (CDocSecurityScheme schemeName scheme) child doc =
  let
    secSchemes = [(schemeName, scheme)] :: Definitions SecurityScheme
    secReqs = [SecurityRequirement [(schemeName, [])]] :: [SecurityRequirement]
   in
    postOrder child doc $ \doc' ->
      doc'
        & securityDefinitions <>~ SecurityDefinitions secSchemes
        & allOperations . security <>~ secReqs
mergeDoc (CDocRequestBody defs mimeList bodyParam) child doc =
  postOrder child doc $ \doc' ->
    doc'
      & allOperations
        %~ ( \op ->
              op
                & parameters %~ (Inline bodyParam :)
                & consumes %~ Just . maybe mimeList (<> mimeList)
           )
      & definitions %~ (<> defs)
mergeDoc (CDocRequestHeader param) child doc =
  postOrder child doc $ \doc' ->
    doc' & allOperations . parameters <>~ [Inline param]
mergeDoc (CDocMethod m) child doc =
  postOrder child doc $ \doc' ->
    doc' & paths %~ Map.map (removeOtherMethods m)
mergeDoc (CDocPathElem path) child doc =
  postOrder child doc $ prependPath (Text.unpack path)
mergeDoc (CDocPathVar param) child doc =
  postOrder child doc $ \doc' ->
    prependPath ("{" <> Text.unpack (_paramName param) <> "}") doc'
      & allOperations . parameters <>~ [Inline param]
mergeDoc (CDocRouteDoc summ descr) child doc =
  postOrder child doc $ \doc' ->
    doc'
      -- keep any existing documentation
      & allOperations . summary %~ (<|> fmap getSummary summ)
      & allOperations . description %~ (<|> fmap getDescription descr)
mergeDoc (CDocQueryParam param) child doc =
  postOrder child doc $ \doc' ->
    doc' & allOperations . parameters <>~ [Inline param]
mergeDoc (CDocStatus status descr) child doc =
  preOrder child doc $ \doc' ->
    let resp =
          mempty @Response
            & description .~ maybe "" getDescription descr
        opr =
          mempty @Operation
            & at (HTTP.statusCode status) ?~ Inline resp
        pathItem =
          mempty @PathItem
            & get ?~ opr
            & put ?~ opr
            & post ?~ opr
            & delete ?~ opr
            & options ?~ opr
            & head_ ?~ opr
            & patch ?~ opr
     in doc' & paths <>~ [("/", pathItem)]
mergeDoc (CDocResponseBody defs mimeList responseSchema) child doc =
  postOrder child doc $ \doc' ->
    let resp = mempty @Response & schema .~ responseSchema
     in doc'
          & allOperations
            %~ ( \op ->
                  op
                    & responses . responses %~ Map.map (`swaggerMappend` Inline resp)
                    & produces %~ Just . maybe mimeList (`swaggerMappend` mimeList)
               )
          & definitions %~ (<> defs)
mergeDoc (CDocResponseHeader headerName header) child doc =
  postOrder child doc $ \doc' ->
    let resp = mempty @Response & headers <>~ [(headerName, header)]
     in doc' & allOperations . responses . responses %~ Map.map (`swaggerMappend` Inline resp)

removeOtherMethods :: HTTP.StdMethod -> PathItem -> PathItem
removeOtherMethods method PathItem{..} =
  case method of
    HTTP.GET -> mempty{_pathItemGet, _pathItemParameters}
    HTTP.PUT -> mempty{_pathItemPut, _pathItemParameters}
    HTTP.POST -> mempty{_pathItemPost, _pathItemParameters}
    HTTP.DELETE -> mempty{_pathItemDelete, _pathItemParameters}
    HTTP.HEAD -> mempty{_pathItemHead, _pathItemParameters}
    HTTP.OPTIONS -> mempty{_pathItemOptions, _pathItemParameters}
    HTTP.PATCH -> mempty{_pathItemPatch, _pathItemParameters}
    -- Swagger does not support CONNECT and TRACE
    HTTP.CONNECT -> mempty{_pathItemParameters}
    HTTP.TRACE -> mempty{_pathItemParameters}
