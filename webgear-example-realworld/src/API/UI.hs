module API.UI where

import API.Common (App)
import Network.Mime (MimeType)
import Relude
import WebGear.Server

assets ::
  ( StdHandler h App
  , Sets
      h
      [ RequiredResponseHeader "Content-Type" MimeType
      , UnknownContentBody
      ]
      Response
  ) =>
  RequestHandler h ts
assets = serveDir "ui/assets" Nothing

index ::
  ( StdHandler h App
  , Sets
      h
      [ RequiredResponseHeader "Content-Type" MimeType
      , UnknownContentBody
      ]
      Response
  ) =>
  RequestHandler h ts
index = proc _ -> serveFile -< "ui/index.html"
