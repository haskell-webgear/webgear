-- | All the traits supported by WebGear.
module WebGear.Core.Traits (
  module WebGear.Core.Trait.Auth.Basic,
  module WebGear.Core.Trait.Auth.JWT,
  module WebGear.Core.Trait.Auth.Common,
  module WebGear.Core.Trait.Body,
  module WebGear.Core.Trait.Header,
  module WebGear.Core.Trait.Method,
  module WebGear.Core.Trait.Path,
  module WebGear.Core.Trait.QueryParam,
  module WebGear.Core.Trait.Status,
  StdHandler,
) where

import WebGear.Core.Handler (Handler)
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Gets, Sets)
import WebGear.Core.Trait.Auth.Basic
import WebGear.Core.Trait.Auth.Common
import WebGear.Core.Trait.Auth.JWT
import WebGear.Core.Trait.Body
import WebGear.Core.Trait.Header
import WebGear.Core.Trait.Method
import WebGear.Core.Trait.Path
import WebGear.Core.Trait.QueryParam
import WebGear.Core.Trait.Status

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
