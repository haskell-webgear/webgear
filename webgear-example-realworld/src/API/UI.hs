module API.UI where

import API.Common (App)
import Network.Wai.Application.Static (StaticSettings (..), defaultWebAppSettings)
import WaiAppStatic.Types (unsafeToPiece)
import WebGear.Server

assets :: (Handler h App) => RequestHandler h ts
assets =
  let settings = (defaultWebAppSettings "ui"){ssIndices = unsafeToPiece <$> ["index.html", "index.htm"]}
   in serveStatic settings
