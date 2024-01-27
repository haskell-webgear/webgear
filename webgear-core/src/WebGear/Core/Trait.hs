{-# LANGUAGE UndecidableInstances #-}

{- | Traits are optional attributes associated with a value. For
 example, a list containing totally ordered values might have a
 @Maximum@ trait where the associated attribute is the maximum
 value. This trait exists only if the list is non-empty. The 'Trait'
 typeclass provides an interface to extract such trait attributes.

 Traits help to associate attributes with values in a type-safe
 manner.

 Traits are somewhat similar to [refinement
 types](https://hackage.haskell.org/package/refined), but allow
 arbitrary attributes to be associated with a value instead of only a
 predicate.

 A value @a@ associated with traits @ts@ is referred to as a witnessed
 value, represented by the type @a \`'With'\` ts@ where @ts@ is a
 type-level list. You can extract a trait attribute from a witnessed
 value with:

 @
 `pick` \@t (`from` witnessedValue)
 @

 The above expression will result in a compile-time error if @t@ is
 not present in the type-level list of the witnessed value's type.

 You can create a witnessed value in a number of ways:

 First, you can use 'wzero' to lift a regular value to a witnessed
 value with no associated traits.

 Second, you can use 'probe' to search for the presence of a trait and
 add it to the witnessed value; this will adjust the type-level list
 accordingly. This is used in cases where the regular value already
 contains the trait attribute which can be extracted using the 'Get'
 typeclass.

 Third, you can use 'plant' to add a trait attribute to a witnessed
 value, thereby extending its type-level list with one more
 trait. This is used in cases where you want to modify the witnessed
 value. This operation requires an implementation of the 'Set'
 typeclass.
-}
module WebGear.Core.Trait (
  -- * Core Types
  Trait (..),
  TraitAbsence (..),
  Prerequisite,
  Get (..),
  Gets,
  Set (..),
  Sets,
  With,

  -- * Associating values with attributes
  wzero,
  wminus,
  unwitness,
  probe,
  plant,

  -- * Retrieve trait attributes from witnessed values
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
class (Trait t a) => TraitAbsence t a where
  -- | Type that indicates that the trait does not exist for a
  -- value. This could be an error message, exception etc.
  type Absence t a :: Type

{- | Indicates the constraints a trait depends upon as a
prerequisite. This is used to assert that a trait @t@ can be
extracted from a value @a@ only if one or more other traits are
present in the trait list @ts@ associated with it.

If a trait does not depend on other traits this can be set to the
empty contraint @()@.
-}
type family Prerequisite (t :: Type) (ts :: [Type]) (a :: Type) :: Constraint

-- | Extract trait attributes from a value.
class (Arrow h, TraitAbsence t a) => Get h t a where
  -- | Attempt to witness the trait attribute from the value @a@.
  getTrait ::
    (Prerequisite t ts a) =>
    -- | The trait to witness
    t ->
    -- | Arrow that attemtps to witness the trait and can possibly
    -- fail
    h (a `With` ts) (Either (Absence t a) (Attribute t a))

-- | Associate a trait attribute on a value
class (Arrow h, Trait t a) => Set h (t :: Type) a where
  -- | Set a trait attribute @t@ on the value @a \`With\` ts@.
  setTrait ::
    -- | The trait to set
    t ->
    -- | A function to generate a witnessed value. This function must
    -- be called by the `setTrait` implementation to generate a
    -- witnessed value.
    (a `With` ts -> a -> Attribute t a -> a `With` (t : ts)) ->
    -- | An arrow that attaches a new trait attribute to a witnessed
    -- value.
    h (a `With` ts, Attribute t a) (a `With` (t : ts))

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

{- | A value associated with a list of traits, referred to as a
witnessed value. Typically, this is used as an infix type constructor:

@
a \`With\` ts
@

where @a@ is a value and @ts@ is a list of traits associated with
that value.

If @t@ is a type present in the list of types @ts@, it is possible to
extract its attribute from a witnessed value:

@
let witnessedValue :: a \`With\` ts
    witnessedValue = ...

let attr :: `Attribute` t a
    attr = `pick` \@t (`from` witnessedValue)
@
-}
data With a (ts :: [Type]) = With
  { attribute :: !(WitnessedAttribute ts a)
  , unwitness :: !a
  -- ^ Retrieve the value
  }

type family WitnessedAttribute (ts :: [Type]) (a :: Type) where
  WitnessedAttribute '[] a = ()
  WitnessedAttribute (t : ts) a = (Attribute t a, WitnessedAttribute ts a)

-- | Lift a value to a witnessed value having no associated traits.
wzero :: a -> a `With` '[]
wzero = With ()
{-# INLINE wzero #-}

-- | Forget the head trait
wminus :: a `With` (t : ts) -> a `With` ts
wminus (With (_, rv) a) = With rv a
{-# INLINE wminus #-}

{- | Attempt to witness an additional trait with a witnessed value. This
 can fail indicating an 'Absence' of the trait.
-}
probe ::
  forall t ts h a.
  (Get h t a, Prerequisite t ts a) =>
  t ->
  h (a `With` ts) (Either (Absence t a) (a `With` (t : ts)))
probe t = proc l -> do
  res <- getTrait t -< l
  arr add -< (l, res)
  where
    add :: (a `With` ts, Either e (Attribute t a)) -> Either e (a `With` (t : ts))
    add (_, Left e) = Left e
    add (With{..}, Right attr) = Right $ With{attribute = (attr, attribute), ..}
{-# INLINE probe #-}

{- | Set a trait attribute on witnessed value to produce another
   witnessed value with the additional trait attached to it.
-}
plant ::
  forall t ts h a.
  (Set h t a) =>
  t ->
  h (a `With` ts, Attribute t a) (a `With` (t : ts))
plant t = proc (l, attr) -> do
  setTrait t add -< (l, attr)
  where
    add :: a `With` ts -> a -> Attribute t a -> a `With` (t : ts)
    add With{..} a' attr = With{attribute = (attr, attribute), unwitness = a'}
{-# INLINE plant #-}

{- | Constraint that proves that the trait @t@ is present in the list
 of traits @ts@.
-}
class HasTrait t ts where
  -- | Get the attribute associated with @t@ from a witnessed
  -- value. See also: 'pick'.
  from :: a `With` ts -> Tagged t (Attribute t a)

instance HasTrait t (t : ts) where
  from :: a `With` (t : ts) -> Tagged t (Attribute t a)
  from (With (lv, _) _) = Tagged lv
  {-# INLINE from #-}

instance {-# OVERLAPPABLE #-} (HasTrait t ts) => HasTrait t (t' : ts) where
  from :: a `With` (t' : ts) -> Tagged t (Attribute t a)
  from l = from $ wminus l
  {-# INLINE from #-}

{- | Retrieve a trait.

 @pick@ is used along with 'from' to retrieve an attribute from a
 witnessed value:

 @
 pick @t (`from` val)
 @
-}
pick :: Tagged t a -> a
pick = untag
{-# INLINE pick #-}

-- For better type errors
instance (TypeError (MissingTrait t)) => HasTrait t '[] where
  from = undefined

-- | Type error for nicer UX of missing traits
type MissingTrait t =
  Text "The value doesn't have the ‘"
    :<>: ShowType t
    :<>: Text "’ trait."
    :$$: Text ""
    :$$: Text "Did you forget to apply an appropriate middleware?"
    :$$: Text "For e.g. The trait ‘Body JSON t’ requires ‘requestBody @t JSON’ middleware."
    :$$: Text ""
    :$$: Text "or did you use a wrong trait type?"
    :$$: Text "For e.g., ‘RequiredQueryParam \"foo\" Int’ instead of ‘RequiredQueryParam \"foo\" String’?"
    :$$: Text ""

{- | Constraint that proves that all the traits in the list @ts@ are
 also present in the list @qs@.
-}
type family HaveTraits ts qs :: Constraint where
  HaveTraits '[] qs = ()
  HaveTraits (t : ts) qs = (HasTrait t qs, HaveTraits ts qs)
