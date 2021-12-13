-- | All the middlewares supported by WebGear.
module WebGear.Core.Middlewares (
  module WebGear.Core.Middleware.Auth.Basic,
  module WebGear.Core.Middleware.Auth.JWT,
  module WebGear.Core.Middleware.Auth.Common,
  module WebGear.Core.Middleware.Body,
  module WebGear.Core.Middleware.Header,
  module WebGear.Core.Middleware.Method,
  module WebGear.Core.Middleware.Path,
  module WebGear.Core.Middleware.QueryParam,
  module WebGear.Core.Middleware.Status,
  StdHandler,
) where

import WebGear.Core.Handler (Handler)
import WebGear.Core.Middleware.Auth.Basic
import WebGear.Core.Middleware.Auth.Common
import WebGear.Core.Middleware.Auth.JWT
import WebGear.Core.Middleware.Body
import WebGear.Core.Middleware.Header
import WebGear.Core.Middleware.Method
import WebGear.Core.Middleware.Path
import WebGear.Core.Middleware.QueryParam
import WebGear.Core.Middleware.Status
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Gets, Sets)

{- | Constraints that include all common traits.

 The type variables are:

 * @h@ - The handler arrow
 * @m@ - The underlying monad of the handler
 * @req@ - List of traits the handler `Gets` from the request
 * @res@ - List of traits the handler `Sets` on the response
-}
type StdHandler h m req res =
  ( Handler h m
  , Gets h [Method, Path, PathEnd] Request
  , Gets h req Request
  , Sets h (Status : res) Response
  )
