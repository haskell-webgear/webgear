{-# OPTIONS_GHC -Wno-unused-imports #-}

{- | WebGear is a library to build composable, type-safe HTTP APIs.

 The modules below have haddock documentation that can be used as
 reference material. If you are completely new to WebGear, a good
 starting point is the [WebGear User
 Guide](https://haskell-webgear.github.io/user_guide/1.0.0/index.html). Example
 programs built using WebGear are available under
 <https://github.com/haskell-webgear/>.

 Importing "WebGear.Core" is a quick way to get everything needed to
 build WebGear API specifications.
-}
module WebGear.Core (
  module Control.Arrow,
  module Data.Text,
  module WebGear.Core.Trait,
  module WebGear.Core.Request,
  module WebGear.Core.Response,
  module WebGear.Core.Modifiers,
  module WebGear.Core.Handler,
  module WebGear.Core.Handler.Static,
  module WebGear.Core.Traits,
  module WebGear.Core.MIMETypes,
) where

import Control.Arrow
import Data.Text
import qualified Network.Wai as Wai
import WebGear.Core.MIMETypes

import WebGear.Core.Handler
import WebGear.Core.Handler.Static
import WebGear.Core.Modifiers
import WebGear.Core.Request
import WebGear.Core.Response
import WebGear.Core.Trait
import WebGear.Core.Traits
