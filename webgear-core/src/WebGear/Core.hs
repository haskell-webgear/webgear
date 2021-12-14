{-# OPTIONS_GHC -Wno-unused-imports #-}

{- |
WebGear is a library to build composable, type-safe HTTP APIs.

The modules below have haddock documentation that can be used as
reference material. If you are completely new to WebGear, a good
starting point is the tutorial in WebGear website. Example programs
built using WebGear are available under
https://github.com/haskell-webgear/.
-}
module WebGear.Core (
  module Control.Arrow,
  module Data.Proxy,
  module Data.Text,
  module Web.HttpApiData,
  module WebGear.Core.Trait,
  module WebGear.Core.Request,
  module WebGear.Core.Response,
  module WebGear.Core.Modifiers,
  module WebGear.Core.Handler,
  module WebGear.Core.Handler.Static,
  module WebGear.Core.Traits,
) where

import Control.Arrow

import Data.Proxy (Proxy (..))
import Data.Text
import Web.HttpApiData (FromHttpApiData (..))

import qualified Network.Wai as Wai

import WebGear.Core.Handler
import WebGear.Core.Handler.Static
import WebGear.Core.Modifiers
import WebGear.Core.Request
import WebGear.Core.Response
import WebGear.Core.Trait

import WebGear.Core.Traits
