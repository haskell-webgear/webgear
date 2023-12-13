module WebGear.Core.MIMEType.PlainText (
  PlainText',
  PlainText,
) where

import Data.Proxy (Proxy (..))
import Data.String (fromString)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Network.HTTP.Media as HTTP
import WebGear.Core.MIMEType (MIMEType (..))

data PlainText' (charset :: Maybe Symbol)

instance MIMEType (PlainText' Nothing) where
  mimeType :: Proxy (PlainText' Nothing) -> HTTP.MediaType
  mimeType _ = "text/plain"

instance (KnownSymbol cs) => MIMEType (PlainText' (Just cs)) where
  mimeType :: Proxy (PlainText' (Just cs)) -> HTTP.MediaType
  mimeType _ = fromString $ "text/plain;charset=" <> symbolVal (Proxy @cs)

-- | The text/plain MIME type with charset set to utf-8
type PlainText = PlainText' (Just "utf8")
