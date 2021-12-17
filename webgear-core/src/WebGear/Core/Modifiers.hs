-- | Various modifiers used by traits
module WebGear.Core.Modifiers (
  Existence (..),
  ParseStyle (..),
  Documentation (..),
) where

import Data.Monoid (First (..))
import Data.Text (Text)

{- | Modifier used to indicate whether a trait is required or
 optional.
-}
data Existence = Required | Optional

{- | Modifier used to indicate whether a trait is parsed strictly or
 leniently.
-}
data ParseStyle = Strict | Lenient

-- | Documentation associated with a trait.
data Documentation = Documentation
  { docSummary :: Maybe Text
  , docDescription :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Read)

instance Semigroup Documentation where
  doc1 <> doc2 =
    Documentation
      { docSummary = getFirst $ First (docSummary doc1) <> First (docSummary doc2)
      , docDescription = getFirst $ First (docDescription doc1) <> First (docDescription doc2)
      }

instance Monoid Documentation where
  mempty = Documentation mempty mempty
