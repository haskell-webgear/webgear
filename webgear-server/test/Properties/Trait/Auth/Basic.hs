module Properties.Trait.Auth.Basic (
  tests,
) where

import Control.Arrow (returnA, (>>>))
import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 (elem)
import Data.Either (fromRight)
import Data.Functor.Identity (Identity, runIdentity)
import Network.Wai (defaultRequest, requestHeaders)
import Test.QuickCheck (
  Discard (..),
  Property,
  allProperties,
  counterexample,
  property,
  (.&&.),
  (===),
 )
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)
import WebGear.Core.Request (Request (..))
import WebGear.Core.Trait (With, getTrait, probe, wzero)
import WebGear.Core.Trait.Auth.Basic (
  BasicAuth,
  BasicAuth' (..),
  Credentials (..),
  Password (..),
  Username (..),
 )
import WebGear.Core.Trait.Auth.Common (AuthorizationHeader)
import WebGear.Core.Trait.Header (Header (..))
import WebGear.Server.Handler (ServerHandler, runServerHandler)
import WebGear.Server.Trait.Auth.Basic ()
import WebGear.Server.Trait.Header ()
import Prelude hiding (elem)

prop_basicAuth :: Property
prop_basicAuth = property f
  where
    f (username, password)
      | ':' `elem` username = property Discard
      | otherwise =
          let hval = "Basic " <> encode (username <> ":" <> password)

              mkRequest :: ServerHandler Identity () (Request `With` '[AuthorizationHeader "Basic"])
              mkRequest = proc () -> do
                let req = Request $ defaultRequest{requestHeaders = [("Authorization", hval)]}
                r <- probe Header -< wzero req
                returnA -< fromRight undefined r

              authCfg :: BasicAuth Identity () Credentials
              authCfg = BasicAuth'{toBasicAttribute = pure . Right}
           in runIdentity $ do
                res <- runServerHandler (mkRequest >>> getTrait authCfg) [""] ()
                pure $ case res of
                  Right (Right creds) ->
                    credentialsUsername creds
                      === Username username
                      .&&. credentialsPassword creds
                      === Password password
                  e -> counterexample ("Unexpected failure: " <> show e) (property False)

-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Auth.Basic" $allProperties
