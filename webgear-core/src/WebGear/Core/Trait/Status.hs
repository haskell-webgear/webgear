-- | Generate responses based on their HTTP status
module WebGear.Core.Trait.Status (
  Status (..),

  -- * Create responses
  mkResponse,
  continue100,
  switchingProtocols101,
  ok200,
  created201,
  accepted202,
  nonAuthoritative203,
  noContent204,
  resetContent205,
  partialContent206,
  multipleChoices300,
  movedPermanently301,
  found302,
  seeOther303,
  notModified304,
  temporaryRedirect307,
  permanentRedirect308,
  badRequest400,
  unauthorized401,
  paymentRequired402,
  forbidden403,
  notFound404,
  methodNotAllowed405,
  notAcceptable406,
  proxyAuthenticationRequired407,
  requestTimeout408,
  conflict409,
  gone410,
  lengthRequired411,
  preconditionFailed412,
  requestEntityTooLarge413,
  requestURITooLong414,
  unsupportedMediaType415,
  requestedRangeNotSatisfiable416,
  expectationFailed417,
  imATeapot418,
  unprocessableEntity422,
  preconditionRequired428,
  tooManyRequests429,
  requestHeaderFieldsTooLarge431,
  internalServerError500,
  notImplemented501,
  badGateway502,
  serviceUnavailable503,
  gatewayTimeout504,
  httpVersionNotSupported505,
  networkAuthenticationRequired511,
) where

import qualified Network.HTTP.Types as HTTP
import WebGear.Core.Response (Response (..), ResponseBody (ResponseBodyBuilder))
import WebGear.Core.Trait (Attribute, Set, With, plant, wzero)

-- | HTTP response status
newtype Status = Status HTTP.Status

type instance Attribute Status Response = HTTP.Status

-- | Generate a response with the specified status
mkResponse :: (Set h Status Response) => HTTP.Status -> h () (Response `With` '[Status])
mkResponse status = proc () -> do
  let response = wzero $ Response status [] (ResponseBodyBuilder mempty)
  plant (Status status) -< (response, status)
{-# INLINE mkResponse #-}

-- | Continue 100 response
continue100 :: (Set h Status Response) => h () (Response `With` '[Status])
continue100 = mkResponse HTTP.continue100
{-# INLINE continue100 #-}

-- | Switching Protocols 101 response
switchingProtocols101 :: (Set h Status Response) => h () (Response `With` '[Status])
switchingProtocols101 = mkResponse HTTP.switchingProtocols101
{-# INLINE switchingProtocols101 #-}

-- | OK 200 response
ok200 :: (Set h Status Response) => h () (Response `With` '[Status])
ok200 = mkResponse HTTP.ok200
{-# INLINE ok200 #-}

-- | Created 201 response
created201 :: (Set h Status Response) => h () (Response `With` '[Status])
created201 = mkResponse HTTP.created201
{-# INLINE created201 #-}

-- | Accepted 202 response
accepted202 :: (Set h Status Response) => h () (Response `With` '[Status])
accepted202 = mkResponse HTTP.accepted202
{-# INLINE accepted202 #-}

-- | Non-Authoritative 203 response
nonAuthoritative203 :: (Set h Status Response) => h () (Response `With` '[Status])
nonAuthoritative203 = mkResponse HTTP.nonAuthoritative203
{-# INLINE nonAuthoritative203 #-}

-- | No Content 204 response
noContent204 :: (Set h Status Response) => h () (Response `With` '[Status])
noContent204 = mkResponse HTTP.noContent204
{-# INLINE noContent204 #-}

-- | Reset Content 205 response
resetContent205 :: (Set h Status Response) => h () (Response `With` '[Status])
resetContent205 = mkResponse HTTP.resetContent205
{-# INLINE resetContent205 #-}

-- | Partial Content 206 response
partialContent206 :: (Set h Status Response) => h () (Response `With` '[Status])
partialContent206 = mkResponse HTTP.partialContent206
{-# INLINE partialContent206 #-}

-- | Multiple Choices 300 response
multipleChoices300 :: (Set h Status Response) => h () (Response `With` '[Status])
multipleChoices300 = mkResponse HTTP.multipleChoices300
{-# INLINE multipleChoices300 #-}

-- | Moved Permanently 301 response
movedPermanently301 :: (Set h Status Response) => h () (Response `With` '[Status])
movedPermanently301 = mkResponse HTTP.movedPermanently301
{-# INLINE movedPermanently301 #-}

-- | Found 302 response
found302 :: (Set h Status Response) => h () (Response `With` '[Status])
found302 = mkResponse HTTP.found302
{-# INLINE found302 #-}

-- | See Other 303 response
seeOther303 :: (Set h Status Response) => h () (Response `With` '[Status])
seeOther303 = mkResponse HTTP.seeOther303
{-# INLINE seeOther303 #-}

-- | Not Modified 304 response
notModified304 :: (Set h Status Response) => h () (Response `With` '[Status])
notModified304 = mkResponse HTTP.notModified304
{-# INLINE notModified304 #-}

-- | Temporary Redirect 307 response
temporaryRedirect307 :: (Set h Status Response) => h () (Response `With` '[Status])
temporaryRedirect307 = mkResponse HTTP.temporaryRedirect307
{-# INLINE temporaryRedirect307 #-}

-- | Permanent Redirect 308 response
permanentRedirect308 :: (Set h Status Response) => h () (Response `With` '[Status])
permanentRedirect308 = mkResponse HTTP.permanentRedirect308
{-# INLINE permanentRedirect308 #-}

-- | Bad Request 400 response
badRequest400 :: (Set h Status Response) => h () (Response `With` '[Status])
badRequest400 = mkResponse HTTP.badRequest400
{-# INLINE badRequest400 #-}

-- | Unauthorized 401 response
unauthorized401 :: (Set h Status Response) => h () (Response `With` '[Status])
unauthorized401 = mkResponse HTTP.unauthorized401
{-# INLINE unauthorized401 #-}

-- | Payment Required 402 response
paymentRequired402 :: (Set h Status Response) => h () (Response `With` '[Status])
paymentRequired402 = mkResponse HTTP.paymentRequired402
{-# INLINE paymentRequired402 #-}

-- | Forbidden 403 response
forbidden403 :: (Set h Status Response) => h () (Response `With` '[Status])
forbidden403 = mkResponse HTTP.forbidden403
{-# INLINE forbidden403 #-}

-- | Not Found 404 response
notFound404 :: (Set h Status Response) => h () (Response `With` '[Status])
notFound404 = mkResponse HTTP.notFound404
{-# INLINE notFound404 #-}

-- | Method Not Allowed 405 response
methodNotAllowed405 :: (Set h Status Response) => h () (Response `With` '[Status])
methodNotAllowed405 = mkResponse HTTP.methodNotAllowed405
{-# INLINE methodNotAllowed405 #-}

-- | Not Acceptable 406 response
notAcceptable406 :: (Set h Status Response) => h () (Response `With` '[Status])
notAcceptable406 = mkResponse HTTP.notAcceptable406
{-# INLINE notAcceptable406 #-}

-- | Proxy Authentication Required 407 response
proxyAuthenticationRequired407 :: (Set h Status Response) => h () (Response `With` '[Status])
proxyAuthenticationRequired407 = mkResponse HTTP.proxyAuthenticationRequired407
{-# INLINE proxyAuthenticationRequired407 #-}

-- | Request Timeout 408 response
requestTimeout408 :: (Set h Status Response) => h () (Response `With` '[Status])
requestTimeout408 = mkResponse HTTP.requestTimeout408
{-# INLINE requestTimeout408 #-}

-- | Conflict 409 response
conflict409 :: (Set h Status Response) => h () (Response `With` '[Status])
conflict409 = mkResponse HTTP.conflict409
{-# INLINE conflict409 #-}

-- | Gone 410 response
gone410 :: (Set h Status Response) => h () (Response `With` '[Status])
gone410 = mkResponse HTTP.gone410
{-# INLINE gone410 #-}

-- | Length Required 411 response
lengthRequired411 :: (Set h Status Response) => h () (Response `With` '[Status])
lengthRequired411 = mkResponse HTTP.lengthRequired411
{-# INLINE lengthRequired411 #-}

-- | Precondition Failed 412 response
preconditionFailed412 :: (Set h Status Response) => h () (Response `With` '[Status])
preconditionFailed412 = mkResponse HTTP.preconditionFailed412
{-# INLINE preconditionFailed412 #-}

-- | Request Entity Too Large 413 response
requestEntityTooLarge413 :: (Set h Status Response) => h () (Response `With` '[Status])
requestEntityTooLarge413 = mkResponse HTTP.requestEntityTooLarge413
{-# INLINE requestEntityTooLarge413 #-}

-- | Request URI Too Long 414 response
requestURITooLong414 :: (Set h Status Response) => h () (Response `With` '[Status])
requestURITooLong414 = mkResponse HTTP.requestURITooLong414
{-# INLINE requestURITooLong414 #-}

-- | Unsupported Media Type 415 response
unsupportedMediaType415 :: (Set h Status Response) => h () (Response `With` '[Status])
unsupportedMediaType415 = mkResponse HTTP.unsupportedMediaType415
{-# INLINE unsupportedMediaType415 #-}

-- | Requested Range Not Satisfiable 416 response
requestedRangeNotSatisfiable416 :: (Set h Status Response) => h () (Response `With` '[Status])
requestedRangeNotSatisfiable416 = mkResponse HTTP.requestedRangeNotSatisfiable416
{-# INLINE requestedRangeNotSatisfiable416 #-}

-- | Expectation Failed 417 response
expectationFailed417 :: (Set h Status Response) => h () (Response `With` '[Status])
expectationFailed417 = mkResponse HTTP.expectationFailed417
{-# INLINE expectationFailed417 #-}

-- | I'm A Teapot 418 response
imATeapot418 :: (Set h Status Response) => h () (Response `With` '[Status])
imATeapot418 = mkResponse HTTP.imATeapot418
{-# INLINE imATeapot418 #-}

-- | Unprocessable Entity 422 response
unprocessableEntity422 :: (Set h Status Response) => h () (Response `With` '[Status])
unprocessableEntity422 = mkResponse HTTP.unprocessableEntity422
{-# INLINE unprocessableEntity422 #-}

-- | Precondition Required 428 response
preconditionRequired428 :: (Set h Status Response) => h () (Response `With` '[Status])
preconditionRequired428 = mkResponse HTTP.preconditionRequired428
{-# INLINE preconditionRequired428 #-}

-- | Too Many Requests 429 response
tooManyRequests429 :: (Set h Status Response) => h () (Response `With` '[Status])
tooManyRequests429 = mkResponse HTTP.tooManyRequests429
{-# INLINE tooManyRequests429 #-}

-- | Request Header Fields Too Large 431 response
requestHeaderFieldsTooLarge431 :: (Set h Status Response) => h () (Response `With` '[Status])
requestHeaderFieldsTooLarge431 = mkResponse HTTP.requestHeaderFieldsTooLarge431
{-# INLINE requestHeaderFieldsTooLarge431 #-}

-- | Internal Server Error 500 response
internalServerError500 :: (Set h Status Response) => h () (Response `With` '[Status])
internalServerError500 = mkResponse HTTP.internalServerError500
{-# INLINE internalServerError500 #-}

-- | Not Implemented 501 response
notImplemented501 :: (Set h Status Response) => h () (Response `With` '[Status])
notImplemented501 = mkResponse HTTP.notImplemented501
{-# INLINE notImplemented501 #-}

-- | Bad Gateway 502 response
badGateway502 :: (Set h Status Response) => h () (Response `With` '[Status])
badGateway502 = mkResponse HTTP.badGateway502
{-# INLINE badGateway502 #-}

-- | Service Unavailable 503 response
serviceUnavailable503 :: (Set h Status Response) => h () (Response `With` '[Status])
serviceUnavailable503 = mkResponse HTTP.serviceUnavailable503
{-# INLINE serviceUnavailable503 #-}

-- | Gateway Timeout 504 response
gatewayTimeout504 :: (Set h Status Response) => h () (Response `With` '[Status])
gatewayTimeout504 = mkResponse HTTP.gatewayTimeout504
{-# INLINE gatewayTimeout504 #-}

-- | HTTP Version Not Supported 505 response
httpVersionNotSupported505 :: (Set h Status Response) => h () (Response `With` '[Status])
httpVersionNotSupported505 = mkResponse HTTP.httpVersionNotSupported505
{-# INLINE httpVersionNotSupported505 #-}

-- | Network Authentication Required 511 response
networkAuthenticationRequired511 :: (Set h Status Response) => h () (Response `With` '[Status])
networkAuthenticationRequired511 = mkResponse HTTP.networkAuthenticationRequired511
{-# INLINE networkAuthenticationRequired511 #-}
