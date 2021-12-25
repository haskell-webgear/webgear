-- | Various modifiers used by traits
module WebGear.Core.Modifiers (
  Existence (..),
  ParseStyle (..),
) where

{- | Modifier used to indicate whether a trait is required or
 optional.
-}
data Existence = Required | Optional

{- | Modifier used to indicate whether a trait is parsed strictly or
 leniently.
-}
data ParseStyle = Strict | Lenient
