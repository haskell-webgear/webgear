{-# LANGUAGE QuasiQuotes #-}

-- | Host swagger-ui based on WebGear API specifications
module WebGear.Swagger.UI (
  swaggerUI,
) where

import Control.Arrow (returnA, (<+>))
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString)
import Data.Text (Text, unpack)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Mime as Mime
import WebGear.Core (
  Body,
  HTML (..),
  Handler (consumeRoute),
  JSON (..),
  PlainText (..),
  RequestHandler,
  RequiredResponseHeader,
  Response (..),
  ResponseBody (ResponseBodyBuilder),
  RoutePath (..),
  Sets,
  StdHandler,
  UnknownContentBody,
  match,
  method,
  ok200,
  pathEnd,
  respondA,
  route,
  setBodyWithoutContentType,
  setHeader,
  unwitnessA,
  (>->),
 )
import qualified WebGear.Swagger.UI.Embedded as Embedded

{- | An API that hosts a few endpoints for swagger-ui.

- @/@ - redirects to @/index.html@
- @/index.html@ - UI entry point for swagger-ui
- @/swagger.json@ - Swagger/OpenAPI specification in json format
- @/...@ - Other UI assets required for swagger-ui
-}
swaggerUI ::
  ( StdHandler h m
  , Sets
      h
      [ RequiredResponseHeader "Content-Type" Text
      , RequiredResponseHeader "Content-Type" Mime.MimeType
      , Body HTML ByteString
      , Body JSON apiSpec
      , UnknownContentBody
      ]
      Response
  ) =>
  -- | Swagger 2.0 or OpenAPI 3.x specification
  apiSpec ->
  RequestHandler h ts
swaggerUI apiSpec =
  rootEndpoint
    <+> indexHtml
    <+> swaggerJson apiSpec
    <+> uiAssets

rootEndpoint ::
  ( StdHandler h m
  , Sets h [RequiredResponseHeader "Content-Type" Text, Body HTML ByteString] Response
  ) =>
  RequestHandler h ts
rootEndpoint = method HTTP.GET $ pathEnd serveIndexHtml

indexHtml ::
  ( StdHandler h m
  , Sets h [RequiredResponseHeader "Content-Type" Text, Body HTML ByteString] Response
  ) =>
  RequestHandler h ts
indexHtml = [route| HTTP.GET /index.html |] serveIndexHtml

serveIndexHtml ::
  ( StdHandler h m
  , Sets h [RequiredResponseHeader "Content-Type" Text, Body HTML ByteString] Response
  ) =>
  RequestHandler h ts
serveIndexHtml = proc _request ->
  respondA HTTP.ok200 HTML -< Embedded.indexHtmlFile

swaggerJson ::
  ( StdHandler h m
  , Sets h [RequiredResponseHeader "Content-Type" Text, Body JSON apiSpec] Response
  ) =>
  -- | Swagger 2.0 or OpenAPI 3.x specification
  apiSpec ->
  RequestHandler h ts
swaggerJson apiSpec =
  [route| HTTP.GET /swagger.json |]
    $ proc _request -> respondA HTTP.ok200 JSON -< apiSpec

uiAssets ::
  ( StdHandler h m
  , Sets
      h
      [ RequiredResponseHeader "Content-Type" Text
      , RequiredResponseHeader "Content-Type" Mime.MimeType
      , UnknownContentBody
      ]
      Response
  ) =>
  RequestHandler h ts
uiAssets =
  [match| HTTP.GET / |]
    $ proc _request -> do
      RoutePath path <- consumeRoute returnA -< ()
      case path of
        [filename] ->
          case lookup (unpack filename) Embedded.uiAssetsDir of
            Nothing -> notFound -< ()
            Just content -> do
              let contentType = Mime.defaultMimeLookup filename
              (ok200 -< ())
                >-> (\resp -> setBodyWithoutContentType -< (resp, ResponseBodyBuilder $ byteString content))
                >-> (\resp -> setHeader @"Content-Type" -< (resp, contentType))
                >-> (\resp -> unwitnessA -< resp)
        _ -> notFound -< ()
  where
    notFound = proc () -> do
      respondA HTTP.notFound404 PlainText -< "" :: Text
