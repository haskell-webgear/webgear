module WebGear.Core.Middlewares
  ( module WebGear.Core.Middleware.Auth.Basic
  , module WebGear.Core.Middleware.Auth.JWT
  , module WebGear.Core.Middleware.Auth.Common
  , module WebGear.Core.Middleware.Body
  , module WebGear.Core.Middleware.Header
  , module WebGear.Core.Middleware.Method
  , module WebGear.Core.Middleware.Path
  , module WebGear.Core.Middleware.QueryParam
  , module WebGear.Core.Middleware.Status
  , StdHandler
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
import WebGear.Core.Trait (Gets, Set)

-- | Constraints that include all common middlewares.
type StdHandler h m = ( Handler h m
                      , Gets h [Method, Path, PathEnd] Request
                      , Set h Status Response
                      )
