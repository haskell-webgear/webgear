{- | An implementation of `Handler` to generate `OpenApi` documentation
 from WebGear API specifications.
-}
module WebGear.OpenApi.Handler (
  OpenApiHandler (..),
  DocNode (..),
  Tree,
  singletonNode,
  nullNode,
  toOpenApi,
) where

import Control.Applicative ((<|>))
import Control.Arrow (Arrow (..), ArrowChoice (..), ArrowPlus (..), ArrowZero (..))
import Control.Arrow.Operations (ArrowError (..))
import qualified Control.Category as Cat
import Control.Lens (at, (%~), (&), (.~), (<>~), (?~))
import qualified Data.HashMap.Strict.InsOrd as Map
import Data.OpenApi
import Data.OpenApi.Internal.Utils (swaggerMappend)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Media.MediaType (MediaType)
import qualified Network.HTTP.Types as HTTP
import WebGear.Core.Handler (Description (..), Handler (..), RouteMismatch, RoutePath (..), Summary (..))

-- | A tree where internal nodes have one or two children.
data Tree a
  = NullNode
  | SingleNode a (Tree a)
  | BinaryNode (Tree a) (Tree a)
  deriving stock (Show)

-- | Different types of documentation elements captured by the handler
data DocNode
  = DocSecurityScheme Text SecurityScheme
  | DocRequestBody (Definitions Schema) RequestBody
  | DocResponseBody (Definitions Schema) MediaType MediaTypeObject
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
  | CDocRequestBody (Definitions Schema) RequestBody
  | CDocResponseBody (Definitions Schema) MediaType MediaTypeObject
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

{- | A handler that captured `OpenApi` documentation of API
 specifications.
-}
newtype OpenApiHandler m a b = OpenApiHandler
  {openApiDoc :: Tree DocNode}

instance Cat.Category (OpenApiHandler m) where
  id :: OpenApiHandler m a a
  id = OpenApiHandler{openApiDoc = NullNode}

  (.) :: OpenApiHandler m b c -> OpenApiHandler m a b -> OpenApiHandler m a c
  OpenApiHandler doc2 . OpenApiHandler doc1 = OpenApiHandler $ insertAsLeaf doc1 doc2
    where
      insertAsLeaf :: Tree DocNode -> Tree DocNode -> Tree DocNode
      insertAsLeaf parent child = case parent of
        NullNode -> child
        SingleNode doc next -> SingleNode doc (insertAsLeaf next child)
        BinaryNode b1 b2 -> BinaryNode (insertAsLeaf b1 child) (insertAsLeaf b2 child)

instance Arrow (OpenApiHandler m) where
  arr :: (a -> b) -> OpenApiHandler m a b
  arr _ = OpenApiHandler{openApiDoc = NullNode}

  first :: OpenApiHandler m b c -> OpenApiHandler m (b, d) (c, d)
  first (OpenApiHandler doc) = OpenApiHandler doc

  second :: OpenApiHandler m b c -> OpenApiHandler m (d, b) (d, c)
  second (OpenApiHandler doc) = OpenApiHandler doc

instance ArrowZero (OpenApiHandler m) where
  zeroArrow :: OpenApiHandler m b c
  zeroArrow = OpenApiHandler{openApiDoc = NullNode}

instance ArrowPlus (OpenApiHandler m) where
  (<+>) :: OpenApiHandler m b c -> OpenApiHandler m b c -> OpenApiHandler m b c
  OpenApiHandler NullNode <+> OpenApiHandler doc = OpenApiHandler doc
  OpenApiHandler doc <+> OpenApiHandler NullNode = OpenApiHandler doc
  OpenApiHandler doc1 <+> OpenApiHandler doc2 = OpenApiHandler $ BinaryNode doc1 doc2

instance ArrowChoice (OpenApiHandler m) where
  left :: OpenApiHandler m b c -> OpenApiHandler m (Either b d) (Either c d)
  left (OpenApiHandler doc) = OpenApiHandler doc

  right :: OpenApiHandler m b c -> OpenApiHandler m (Either d b) (Either d c)
  right (OpenApiHandler doc) = OpenApiHandler doc

  (+++) :: OpenApiHandler m b c -> OpenApiHandler m b' c' -> OpenApiHandler m (Either b b') (Either c c')
  OpenApiHandler doc +++ OpenApiHandler NullNode = OpenApiHandler doc
  OpenApiHandler NullNode +++ OpenApiHandler doc = OpenApiHandler doc
  OpenApiHandler doc1 +++ OpenApiHandler doc2 = OpenApiHandler $ BinaryNode doc1 doc2

  (|||) :: OpenApiHandler m b d -> OpenApiHandler m c d -> OpenApiHandler m (Either b c) d
  OpenApiHandler doc ||| OpenApiHandler NullNode = OpenApiHandler doc
  OpenApiHandler NullNode ||| OpenApiHandler doc = OpenApiHandler doc
  OpenApiHandler doc1 ||| OpenApiHandler doc2 = OpenApiHandler $ BinaryNode doc1 doc2

instance ArrowError RouteMismatch (OpenApiHandler m) where
  {-# INLINEABLE raise #-}
  raise = OpenApiHandler{openApiDoc = NullNode}

  {-# INLINEABLE handle #-}
  OpenApiHandler doc1 `handle` OpenApiHandler doc2 = OpenApiHandler $ BinaryNode doc1 doc2

  {-# INLINEABLE tryInUnless #-}
  tryInUnless (OpenApiHandler doc1) (OpenApiHandler doc2) (OpenApiHandler doc3) =
    OpenApiHandler $ BinaryNode (BinaryNode doc1 doc2) doc3

instance Monad m => Handler (OpenApiHandler m) m where
  {-# INLINEABLE arrM #-}
  arrM :: (a -> m b) -> OpenApiHandler m a b
  arrM _ = OpenApiHandler{openApiDoc = NullNode}

  {-# INLINEABLE consumeRoute #-}
  consumeRoute :: OpenApiHandler m RoutePath a -> OpenApiHandler m () a
  consumeRoute (OpenApiHandler doc) = OpenApiHandler doc

  {-# INLINEABLE setDescription #-}
  setDescription :: Description -> OpenApiHandler m a a
  setDescription = OpenApiHandler . singletonNode . DocDescription

  {-# INLINEABLE setSummary #-}
  setSummary :: Summary -> OpenApiHandler m a a
  setSummary = OpenApiHandler . singletonNode . DocSummary

-- | Generate OpenApi documentation from a handler
toOpenApi :: OpenApiHandler m a b -> OpenApi
toOpenApi = go . compact . openApiDoc
  where
    go t = case t of
      NullNode -> mempty
      SingleNode parent child -> mergeDoc parent child mempty
      BinaryNode t1 t2 -> go t1 `combineOpenApi` go t2

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
    compactDoc (DocRequestBody defs body) child =
      let (descr, summ, child') = go child
          body' = body & description .~ fmap getDescription descr
       in (Nothing, summ, SingleNode (CDocRequestBody defs body') child')
    compactDoc (DocResponseBody defs mediaType mediaTypeObject) child =
      SingleNode (CDocResponseBody defs mediaType mediaTypeObject) <$> go child
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

postOrder :: Tree CompactDocNode -> OpenApi -> (OpenApi -> OpenApi) -> OpenApi
postOrder NullNode doc f = f doc
postOrder (SingleNode node child) doc f = f $ mergeDoc node child doc
postOrder (BinaryNode t1 t2) doc f =
  f $ postOrder t1 doc id `combineOpenApi` postOrder t2 doc id

preOrder :: Tree CompactDocNode -> OpenApi -> (OpenApi -> OpenApi) -> OpenApi
preOrder NullNode doc f = f doc
preOrder (SingleNode node child) doc f = mergeDoc node child (f doc)
preOrder (BinaryNode t1 t2) doc f =
  let doc' = f doc
   in postOrder t1 doc' id `combineOpenApi` postOrder t2 doc' id

combinePathItem :: PathItem -> PathItem -> PathItem
combinePathItem s t =
  PathItem
    { _pathItemGet = _pathItemGet s <> _pathItemGet t
    , _pathItemPut = _pathItemPut s <> _pathItemPut t
    , _pathItemPost = _pathItemPost s <> _pathItemPost t
    , _pathItemDelete = _pathItemDelete s <> _pathItemDelete t
    , _pathItemOptions = _pathItemOptions s <> _pathItemOptions t
    , _pathItemHead = _pathItemHead s <> _pathItemHead t
    , _pathItemPatch = _pathItemPatch s <> _pathItemPatch t
    , _pathItemTrace = _pathItemTrace s <> _pathItemTrace t
    , _pathItemParameters = _pathItemParameters s <> _pathItemParameters t
    , _pathItemSummary = _pathItemSummary s <|> _pathItemSummary t
    , _pathItemDescription = _pathItemDescription s <|> _pathItemDescription t
    , _pathItemServers = _pathItemServers s <> _pathItemServers t
    }

combineOpenApi :: OpenApi -> OpenApi -> OpenApi
combineOpenApi s t =
  OpenApi
    { _openApiInfo = _openApiInfo s <> _openApiInfo t
    , _openApiServers = _openApiServers s <> _openApiServers t
    , _openApiPaths = Map.unionWith combinePathItem (_openApiPaths s) (_openApiPaths t)
    , _openApiComponents = _openApiComponents s <> _openApiComponents t
    , _openApiSecurity = _openApiSecurity s <> _openApiSecurity t
    , _openApiTags = _openApiTags s <> _openApiTags t
    , _openApiExternalDocs = _openApiExternalDocs s <|> _openApiExternalDocs t
    }

mergeDoc :: CompactDocNode -> Tree CompactDocNode -> OpenApi -> OpenApi
mergeDoc (CDocSecurityScheme schemeName scheme) child doc =
  let secSchemes = [(schemeName, scheme)] :: Definitions SecurityScheme
      secReqs = [SecurityRequirement [(schemeName, [])]] :: [SecurityRequirement]
   in postOrder child doc $ \doc' ->
        doc'
          & components . securitySchemes <>~ secSchemes
          & allOperations . security <>~ secReqs
mergeDoc (CDocRequestBody defs body) child doc =
  postOrder child doc $ \doc' ->
    doc'
      & allOperations . requestBody ?~ Inline body
      & components . schemas %~ (<> defs)
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
            & trace ?~ opr
     in doc' & paths <>~ [("/", pathItem)]
mergeDoc (CDocResponseBody defs mediaType mediaTypeObject) child doc =
  postOrder child doc $ \doc' ->
    let resp = mempty @Response & content <>~ [(mediaType, mediaTypeObject)]
     in doc'
          & allOperations . responses . responses %~ Map.map (`swaggerMappend` Inline resp)
          & components . schemas %~ (<> defs)
mergeDoc (CDocResponseHeader headerName header) child doc =
  postOrder child doc $ \doc' ->
    let resp = mempty @Response & headers <>~ [(headerName, Inline header)]
     in doc' & allOperations . responses . responses %~ Map.map (`swaggerMappend` Inline resp)

removeOtherMethods :: HTTP.StdMethod -> PathItem -> PathItem
removeOtherMethods method PathItem{..} =
  case method of
    HTTP.GET -> mempty{_pathItemGet, _pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
    HTTP.PUT -> mempty{_pathItemPut, _pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
    HTTP.POST -> mempty{_pathItemPost, _pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
    HTTP.DELETE -> mempty{_pathItemDelete, _pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
    HTTP.HEAD -> mempty{_pathItemHead, _pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
    HTTP.TRACE -> mempty{_pathItemTrace, _pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
    HTTP.OPTIONS -> mempty{_pathItemOptions, _pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
    HTTP.PATCH -> mempty{_pathItemPatch, _pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
    -- OpenApi does not support CONNECT
    HTTP.CONNECT -> mempty{_pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
