{-# LANGUAGE TemplateHaskell #-}

module WebGear.Swagger.UI.Embedded (
  indexHtmlFile,
  uiAssetsDir,
) where

import Data.ByteString (ByteString)
import qualified Data.FileEmbed as FileEmbed

indexHtmlFile :: ByteString
indexHtmlFile = $(FileEmbed.embedFile "index.html")

uiAssetsDir :: [(FilePath, ByteString)]
uiAssetsDir = $(FileEmbed.embedDir "swagger-ui-5.10.5/dist")
