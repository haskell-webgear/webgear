{-# LANGUAGE UndecidableInstances #-}

{- | Traits are optional attributes associated with a value. For
 example, a list containing totally ordered values might have a
 @Maximum@ trait where the associated attribute is the maximum
 value. This trait exists only if the list is non-empty. The `Trait`
 typeclass provides an interface to extract such trait attributes.

 Traits help to link attributes with values in a type-safe manner.

 Traits are somewhat similar to [refinement
 types](https://hackage.haskell.org/package/refined), but allow
 arbitrary attributes to be associated with a value instead of only
 a predicate.
-}
module WebGear.Core.Trait (
  -- * Core Types
  Trait (..),
  TraitAbsence (..),
  Get (..),
  Gets,
  Linked,
  Set (..),
  Sets,

  -- * Linking values with attributes
  linkzero,
  linkminus,
  unlink,
  probe,
  plant,

  -- * Retrive trait attributes from linked values
  HasTrait (..),
  HaveTraits,
  pick,
  MissingTrait,
) where

import Control.Arrow (Arrow (..))
import Data.Kind (Constraint, Type)
import Data.Tagged (Tagged (..), untag)
import GHC.TypeLits (ErrorMessage (..), TypeError)

-- | A trait is an attribute @t@ associated with a value @a@.
class Trait (t :: Type) a where
  -- | Type of the associated attribute when the trait holds for a
  -- value
  type Attribute t a :: Type

-- | A trait @t@ that can be retrieved from @a@ but could be absent.
class Trait t a => TraitAbsence t a where
  -- | Type that indicates that the trait does not exist for a
  -- value. This could be an error message, exception etc.
  type Absence t a :: Type

-- | Extract trait attributes from a value.
class (Arrow h, TraitAbsence t a) => Get h t a where
  -- | Attempt to deduce the trait attribute from the value @a@.
  getTrait ::
    -- | The trait to extract
    t ->
    -- | Arrow that extracts the trait and can possibly fail
    h (Linked ts a) (Either (Absence t a) (Attribute t a))

-- | Set a trait attribute on a value
class (Arrow h, Trait t a) => Set h (t :: Type) a where
  -- | Set a trait attribute @t@ on the value @a@.
  setTrait ::
    -- | The trait to set
    t ->
    -- | A function to generate a linked value. This function must be
    -- called by the `setTrait` implementation to generate a linked
    -- value.
    (Linked ts a -> a -> Attribute t a -> Linked (t : ts) a) ->
    -- | An arrow that attches a new trait attribute to a value linked
    -- with other traits
    h (Linked ts a, Attribute t a) (Linked (t : ts) a)

{- | @Gets h ts a@ is equivalent to @(Get h t1 a, Get h t2 a, ..., Get
 h tn a)@ where @ts = [t1, t2, ..., tn]@.
-}
type family Gets h ts a :: Constraint where
  Gets h '[] a = ()
  Gets h (t : ts) a = (Get h t a, Gets h ts a)

{- | @Sets h ts a@ is equivalent to @(Set h t1 a, Set h t2 a, ..., Set
 h tn a)@ where @ts = [t1, t2, ..., tn]@.
-}
type family Sets h ts a :: Constraint where
  Sets h '[] a = ()
  Sets h (t : ts) a = (Set h t a, Sets h ts a)

-- | A value linked with a type-level list of traits.
data Linked (ts :: [Type]) a = Linked
  { linkAttribute :: !(LinkedAttributes ts a)
  , unlink :: !a
  -- ^ Retrive the value from a linked value
  }

type family LinkedAttributes (ts :: [Type]) (a :: Type) where
  LinkedAttributes '[] a = ()
  LinkedAttributes (t : ts) a = (Attribute t a, LinkedAttributes ts a)

-- | Wrap a value with an empty list of traits.
linkzero :: a -> Linked '[] a
linkzero = Linked ()
{-# INLINE linkzero #-}

-- | Forget the head trait
linkminus :: Linked (t : ts) a -> Linked ts a
linkminus (Linked (_, rv) a) = Linked rv a
{-# INLINE linkminus #-}

{- | Attempt to link an additional trait with an already linked
 value. This can fail indicating an 'Absence' of the trait.
-}
probe ::
  forall t ts h a.
  Get h t a =>
  t ->
  h (Linked ts a) (Either (Absence t a) (Linked (t : ts) a))
probe t = proc l -> do
  res <- getTrait t -< l
  arr link -< (l, res)
  where
    link :: (Linked ts a, Either e (Attribute t a)) -> Either e (Linked (t : ts) a)
    link (_, Left e) = Left e
    link (Linked{..}, Right attr) = Right $ Linked{linkAttribute = (attr, linkAttribute), ..}
{-# INLINE probe #-}

{- | Set a trait attribute on linked value to produce another linked
 value
-}
plant :: forall t ts h a. Set h t a => t -> h (Linked ts a, Attribute t a) (Linked (t : ts) a)
plant t = proc (l, attr) -> do
  setTrait t link -< (l, attr)
  where
    link :: Linked ts a -> a -> Attribute t a -> Linked (t : ts) a
    link Linked{..} a' attr = Linked{linkAttribute = (attr, linkAttribute), unlink = a'}
{-# INLINE plant #-}

{- | Constraint that proves that the trait @t@ is present in the list
 of traits @ts@.
-}
class HasTrait t ts where
  -- | Get the attribute associated with @t@ from a linked value. See also: 'pick'.
  from :: Linked ts a -> Tagged t (Attribute t a)

instance HasTrait t (t : ts) where
  from :: Linked (t : ts) a -> Tagged t (Attribute t a)
  from (Linked (lv, _) _) = Tagged lv
  {-# INLINE from #-}

instance {-# OVERLAPPABLE #-} HasTrait t ts => HasTrait t (t' : ts) where
  from :: Linked (t' : ts) a -> Tagged t (Attribute t a)
  from l = from $ linkminus l
  {-# INLINE from #-}

{- | Retrieve a trait.

 @pick@ is used along with `from` to retrieve an attribute from a
 linked value:

 > pick @t $ from val
-}
pick :: Tagged t a -> a
pick = untag
{-# INLINE pick #-}

-- For better type errors
instance TypeError (MissingTrait t) => HasTrait t '[] where
  from = undefined

-- | Type error for nicer UX of missing traits
type MissingTrait t =
  Text "The request doesn't have the trait ‘" :<>: ShowType t :<>: Text "’."
    :$$: Text ""
    :$$: Text "Did you use a wrong trait type?"
    :$$: Text "For e.g., ‘QueryParam \"foo\" Int’ instead of ‘QueryParam \"foo\" String’?"
    :$$: Text ""
    :$$: Text "Or did you forget to apply an appropriate middleware?"
    :$$: Text "For e.g. The trait ‘JSONBody t’ can be used with ‘jsonRequestBody @t’ middleware."
    :$$: Text ""

{- | Constraint that proves that all the traits in the list @ts@ are
 also present in the list @qs@.
-}
type family HaveTraits ts qs :: Constraint where
  HaveTraits '[] qs = ()
  HaveTraits (t : ts) qs = (HasTrait t qs, HaveTraits ts qs)
