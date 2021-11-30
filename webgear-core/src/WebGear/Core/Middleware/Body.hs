-- |
-- Copyright        : (c) Raghu Kaippully, 2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Handle request body payloads
--
module WebGear.Core.Middleware.Body
  ( -- * Traits
    Body (..)
  , JSONBody' (..)
  , JSONBody
  , MkMediaType (..)

    -- * Middlewares
  , requestBody
  , jsonRequestBody'
  , jsonRequestBody
  , setResponseBody
  , setJSONResponseBody'
  , setJSONResponseBody
  ) where

import Control.Arrow (ArrowChoice)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Network.HTTP.Media as HTTP
import WebGear.Core.Handler (Middleware)
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Get, Linked, Set, Trait (..), TraitAbsence (..), plant, probe)


-- | Body with a type @body@ that can be converted to a 'ByteString'.
data Body (mediaType :: Maybe Symbol) (t :: Type) = Body

instance Trait (Body mt t) Request where
  type Attribute (Body mt t) Request = t

instance TraitAbsence (Body mt t) Request where
  type Absence (Body mt t) Request = Text

instance Trait (Body mt t) Response where
  type Attribute (Body mt t) Response = t

-- | A 'Trait' for converting a JSON formatted body into a value.
data JSONBody' (mediaType :: Maybe Symbol) (t :: Type) = JSONBody'

type JSONBody = JSONBody' (Just "application/json")

instance Trait (JSONBody' mt t) Request where
  type Attribute (JSONBody' mt t) Request = t

instance TraitAbsence (JSONBody' mt t) Request where
  type Absence (JSONBody' mt t) Request = Text

instance Trait (JSONBody' mt t) Response where
  type Attribute (JSONBody' mt t) Response = t


class MkMediaType (a :: Maybe Symbol) where
  mkMediaType :: Proxy a -> Maybe HTTP.MediaType

instance MkMediaType Nothing where
  mkMediaType _ = Nothing

instance KnownSymbol s => MkMediaType (Just s) where
  mkMediaType _ = Just $ fromString $ symbolVal $ Proxy @s


requestBody :: (Get h (Body mt t) Request, ArrowChoice h)
            => h (Linked req Request, Text) Response  -- ^ error handler
            -> Middleware h req (Body mt t : req)
requestBody errorHandler nextHandler = proc request -> do
  result <- probe Body -< request
  case result of
    Left err -> errorHandler -< (request, err)
    Right t -> nextHandler -< t

-- | Parse the request body as JSON and convert it to a value via a
-- 'FromJSON' instance.
--
-- The @okHandler@ is invoked when the body is parsed successfully and
-- the @errorHandler@ is invoked when there is a parsing failure.
--
-- Usage for a type @t@ which has a 'FromJSON' instance:
--
-- > jsonRequestBody @t errorHandler
--
jsonRequestBody' :: forall mt t h req. (Get h (JSONBody' mt t) Request, ArrowChoice h)
                 => h (Linked req Request, Text) Response  -- ^ error handler
                 -> Middleware h req (JSONBody' mt t : req)
jsonRequestBody' errorHandler nextHandler = proc request -> do
  result <- probe JSONBody' -< request
  case result of
    Left err -> errorHandler -< (request, err)
    Right t -> nextHandler -< t

-- | Same as 'jsonRequestBody'' but with a media type @application/json@.
jsonRequestBody :: forall t h req. (Get h (JSONBody t) Request, ArrowChoice h)
                 => h (Linked req Request, Text) Response  -- ^ error handler
                 -> Middleware h req (JSONBody t : req)
jsonRequestBody = jsonRequestBody'

setResponseBody :: forall mt body h ts. Set h (Body mt body) Response
                => h (Linked ts Response, body) (Linked (Body mt body : ts) Response)
setResponseBody = plant Body

setJSONResponseBody' :: forall mt body h ts. Set h (JSONBody' mt body) Response
                    => h (Linked ts Response, body) (Linked (JSONBody' mt body : ts) Response)
setJSONResponseBody' = plant JSONBody'

setJSONResponseBody :: forall body h ts. Set h (JSONBody body) Response
                    => h (Linked ts Response, body) (Linked (JSONBody body : ts) Response)
setJSONResponseBody = setJSONResponseBody'
