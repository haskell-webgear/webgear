{-# LANGUAGE TemplateHaskell #-}

-- | Embed a swagger-ui distribution in Haskell.
module WebGear.Swagger.UI.Embedded (
  indexHtmlFile,
  uiAssetsDir,
) where

import Data.ByteString (ByteString)
import qualified Data.FileEmbed as FileEmbed

-- | Contents of index.html from a swagger-ui distribution
indexHtmlFile :: ByteString
indexHtmlFile = $(FileEmbed.embedFile "index.html")

-- | UI assets from a swagger-ui distribution indexed by their filename
uiAssetsDir :: [(FilePath, ByteString)]
uiAssetsDir = $(FileEmbed.embedDir "swagger-ui-5.10.5/dist")
