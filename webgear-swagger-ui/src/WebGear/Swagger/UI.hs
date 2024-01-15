{-# LANGUAGE QuasiQuotes #-}

-- | Host swagger-ui based on WebGear API specifications
module WebGear.Swagger.UI (
  swaggerUI,
) where

import Control.Arrow ((<+>))
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Mime as Mime
import Network.Wai.Application.Static (embeddedSettings)
import WebGear.Core (
  Body,
  HTML (..),
  JSON (..),
  RequestHandler,
  RequiredResponseHeader,
  Response (..),
  Sets,
  StdHandler,
  UnknownContentBody,
  match,
  method,
  pathEnd,
  respondA,
  route,
  serveStatic,
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
  [route| HTTP.GET /swagger.json |] $
    proc _request -> respondA HTTP.ok200 JSON -< apiSpec

uiAssets :: (StdHandler h m) => RequestHandler h ts
uiAssets =
  [match| HTTP.GET / |] $
    serveStatic (embeddedSettings Embedded.uiAssetsDir)
