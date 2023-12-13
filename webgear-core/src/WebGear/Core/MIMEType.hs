-- | MIME types for HTTP bodies
module WebGear.Core.MIMEType (
  MIMEType (..),
  MIMETypes (..),
) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import qualified Network.HTTP.Media as HTTP

-- | Instances of this class represent MIME types of the resources
class MIMEType (t :: Type) where
  -- | Used for the "Accept" header of the request and the
  -- "Content-Type" header of the response.
  mimeType :: Proxy t -> HTTP.MediaType

-- | A list of 'MIMEType's
class MIMETypes (ts :: [Type]) where
  mimeTypes :: Proxy ts -> [HTTP.MediaType]

instance MIMETypes '[] where
  mimeTypes :: Proxy '[] -> [HTTP.MediaType]
  mimeTypes _ = []

instance (MIMEType t, MIMETypes ts) => MIMETypes (t : ts) where
  mimeTypes :: Proxy (t : ts) -> [HTTP.MediaType]
  mimeTypes _ = mimeType (Proxy @t) : mimeTypes (Proxy @ts)
