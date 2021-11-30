-- |
-- Copyright        : (c) Raghu Kaippully, 2020-2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
module WebGear.Core.Modifiers
  ( Existence (..)
  , ParseStyle (..)
  ) where


-- | Modifier used to indicate whether a trait is required or
-- optional.
data Existence = Required | Optional

-- | Modifier used to indicate whether a trait is parsed strictly or
-- leniently.
data ParseStyle = Strict | Lenient
