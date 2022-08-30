{- | Middlewares related to request paths.

 All the middlewares below attempt to match components of the request
 path. In case of a mismatch, they abandon the current handler and
 tries the next handler.
-}
module WebGear.Core.Trait.Path (
  Path (..),
  PathVar (..),
  PathVarError (..),
  PathEnd (..),
  path,
  pathVar,
  pathEnd,
  match,
  route,
) where

import Control.Arrow (ArrowChoice (..), (>>>))
import Control.Arrow.Operations (ArrowError)
import Data.Function ((&))
import Data.Kind (Type)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..), filter, toList)
import Data.Text (Text)
import GHC.TypeLits (Symbol)
import Language.Haskell.TH (appE, conE)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp (..), Lit (..), Q, TyLit (StrTyLit), Type (..), mkName)
import WebGear.Core.Handler (Middleware, RouteMismatch, routeMismatch)
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get, Trait (..), TraitAbsence (..), probe)
import WebGear.Core.Trait.Method (method)
import Prelude hiding (drop, filter, take)

{- | A path component which is literally matched against the request
 but discarded after that.
-}
newtype Path = Path Text

instance Trait Path Request where
  type Attribute Path Request = ()

instance TraitAbsence Path Request where
  type Absence Path Request = ()

{- | A path variable that is extracted and converted to a value of
 type @val@. The @tag@ is usually a type-level symbol (string) to
 uniquely identify this variable.
-}
data PathVar (tag :: Symbol) (val :: Data.Kind.Type) = PathVar

-- | Failure to extract a 'PathVar'
data PathVarError = PathVarNotFound | PathVarParseError Text
  deriving stock (Eq, Show, Read)

instance Trait (PathVar tag val) Request where
  type Attribute (PathVar tag val) Request = val

instance TraitAbsence (PathVar tag val) Request where
  type Absence (PathVar tag val) Request = PathVarError

-- | Trait to indicate that no more path components are present in the request
data PathEnd = PathEnd

instance Trait PathEnd Request where
  type Attribute PathEnd Request = ()

instance TraitAbsence PathEnd Request where
  type Absence PathEnd Request = ()

{- | A middleware that literally matches path @s@.

 The symbol @s@ could contain one or more parts separated by a
 forward slash character. The route will be rejected if there is no
 match.

 For example, the following code could be used to match the URL path
 \"a\/b\/c\" and then invoke @handler@:

 > path "a/b/c" handler
-}
path ::
  (Get h Path Request, ArrowChoice h, ArrowError RouteMismatch h) =>
  Text ->
  Middleware h req (Path : req)
path s nextHandler = probe (Path s) >>> routeMismatch ||| nextHandler
{-# INLINE path #-}

{- | A middleware that captures a path variable from a single path
 component.

 The value captured is converted to a value of type @val@. The route
 will be rejected if the value is not found or cannot be converted.

 For example, the following code could be used to read a path
 component as 'Int' tagged with the symbol \"objId\", and then
 invoke @handler@:

 > pathVar @"objId" @Int handler
-}
pathVar ::
  forall tag val h req.
  (Get h (PathVar tag val) Request, ArrowChoice h, ArrowError RouteMismatch h) =>
  Middleware h req (PathVar tag val : req)
pathVar nextHandler = probe PathVar >>> routeMismatch ||| nextHandler
{-# INLINE pathVar #-}

-- | A middleware that verifies that end of path is reached.
pathEnd ::
  (Get h PathEnd Request, ArrowChoice h, ArrowError RouteMismatch h) =>
  Middleware h req (PathEnd : req)
pathEnd nextHandler = probe PathEnd >>> routeMismatch ||| nextHandler
{-# INLINE pathEnd #-}

{- | Produces middleware(s) to match an optional HTTP method and some
 path components.

 This middleware matches a prefix of path components, the remaining
 components can be matched by subsequent uses of 'match'.

 This quasiquoter can be used in several ways:

 +---------------------------------------+---------------------------------------------------------------------------------+
 | QuasiQuoter                           | Equivalent Middleware                                                           |
 +=======================================+=================================================================================+
 | @[match| \/a\/b\/c |]@                | @'path' \"\/a\/b\/c\"@                                                          |
 +---------------------------------------+---------------------------------------------------------------------------------+
 | @[match| \/a\/b\/objId:Int\/d |]@     | @'path' \"\/a\/b\" . 'pathVar' \@\"objId\" \@Int . 'path' \"d\"@                |
 +---------------------------------------+---------------------------------------------------------------------------------+
 | @[match| GET \/a\/b\/c |]@            | @'method' GET . 'path' \"\/a\/b\/c\"@                                           |
 +---------------------------------------+---------------------------------------------------------------------------------+
 | @[match| GET \/a\/b\/objId:Int\/d |]@ | @'method' GET . 'path' \"\/a\/b\" . 'pathVar' \@\"objId\" \@Int . 'path' \"d\"@ |
 +---------------------------------------+---------------------------------------------------------------------------------+
-}
match :: QuasiQuoter
match =
  QuasiQuoter
    { quoteExp = toMatchExp
    , quotePat = const $ fail "match cannot be used in a pattern"
    , quoteType = const $ fail "match cannot be used in a type"
    , quoteDec = const $ fail "match cannot be used in a declaration"
    }

{- | Produces middleware(s) to match an optional HTTP method and the
 entire request path.

 This middleware is intended to be used in cases where the entire
 path needs to be matched. Use 'match' middleware to match only an
 initial portion of the path.

 This quasiquoter can be used in several ways:

 +---------------------------------------+----------------------------------------------------------------------------------------------+
 | QuasiQuoter                           | Equivalent Middleware                                                                        |
 +=======================================+==============================================================================================+
 | @[route| \/a\/b\/c |]@                | @'path' \"\/a\/b\/c\" . 'pathEnd'@                                                           |
 +---------------------------------------+----------------------------------------------------------------------------------------------+
 | @[route| \/a\/b\/objId:Int\/d |]@     | @'path' \"\/a\/b\" . 'pathVar' \@\"objId\" \@Int . 'path' \"d\" . 'pathEnd'@                 |
 +---------------------------------------+----------------------------------------------------------------------------------------------+
 | @[route| GET \/a\/b\/c |]@            | @'method' GET . 'path' \"\/a\/b\/c\" . 'pathEnd'@                                            |
 +---------------------------------------+----------------------------------------------------------------------------------------------+
 | @[route| GET \/a\/b\/objId:Int\/d |]@ | @'method' GET . 'path' \"\/a\/b\" . 'pathVar' \@\"objId\" \@Int . 'path' \"d\" . 'pathEnd')@ |
 +---------------------------------------+----------------------------------------------------------------------------------------------+
-}
route :: QuasiQuoter
route =
  QuasiQuoter
    { quoteExp = toRouteExp
    , quotePat = const $ fail "route cannot be used in a pattern"
    , quoteType = const $ fail "route cannot be used in a type"
    , quoteDec = const $ fail "route cannot be used in a declaration"
    }

toRouteExp :: String -> Q Exp
toRouteExp s = do
  e <- toMatchExp s
  pure $ compose e (VarE 'pathEnd)

toMatchExp :: String -> Q Exp
toMatchExp s = case List.words s of
  [m, p] -> toMethodAndPathExps m p
  [p] -> do
    pathExps <- toPathExps p
    pure $ List.foldr1 compose pathExps
  _ -> fail "Expected an HTTP method and a path or just a path"
  where
    toMethodAndPathExps :: String -> String -> Q Exp
    toMethodAndPathExps m p = do
      methodExp <- [|method|] `appE` conE (mkName m)
      pathExps <- toPathExps p
      pure $ List.foldr1 compose $ methodExp :| pathExps

    toPathExps :: String -> Q [Exp]
    toPathExps p =
      splitOn '/' p
        & filter (/= "")
        & fmap (splitOn ':')
        & List.foldr joinPath []
        & mapM toPathExp

    joinPath :: NonEmpty String -> [NonEmpty String] -> [NonEmpty String]
    joinPath p [] = [p]
    joinPath (p :| []) ((p' :| []) : xs) = ((p <> "/" <> p') :| []) : xs
    joinPath y (x : xs) = y : x : xs

    toPathExp :: NonEmpty String -> Q Exp
    toPathExp (p :| []) = pure $ AppE (VarE 'path) (LitE $ StringL p)
    toPathExp (v :| [t]) = pure $ AppTypeE (AppTypeE (VarE 'pathVar) (LitT $ StrTyLit v)) (ConT $ mkName t)
    toPathExp xs = fail $ "Invalid path component: " <> List.intercalate ":" (toList xs)

compose :: Exp -> Exp -> Exp
compose l = UInfixE l (VarE $ mkName ".")

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr f ([] :| [])
  where
    f x acc | x == sep = [] :| toList acc
    f x (y :| ys) = (x : y) :| ys
