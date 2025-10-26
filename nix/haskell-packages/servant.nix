{ mkDerivation, aeson, attoparsec, base, bifunctors, bytestring
, case-insensitive, constraints, containers, deepseq, generics-sop
, hspec, hspec-discover, http-api-data, http-media, http-types, lib
, mmorph, mtl, network-uri, QuickCheck, quickcheck-instances
, singleton-bool, sop-core, text, transformers, vault
}:
mkDerivation {
  pname = "servant";
  version = "0.20.3.0";
  sha256 = "f273e65c8f6cc6ddfa204c8d1d0bc5b7e258cec288ca5b2a2e57fadb31bf6602";
  revision = "2";
  editedCabalFile = "0wvq6jj6js7sxq1rrn4v6749zfwkz3cl8dsypf5cvbpkz1qp4d7j";
  libraryHaskellDepends = [
    aeson attoparsec base bifunctors bytestring case-insensitive
    constraints containers deepseq generics-sop http-api-data
    http-media http-types mmorph mtl network-uri QuickCheck
    singleton-bool sop-core text transformers vault
  ];
  testHaskellDepends = [
    aeson base bytestring hspec http-media mtl network-uri QuickCheck
    quickcheck-instances text
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://docs.servant.dev/";
  description = "A family of combinators for defining webservices APIs";
  license = lib.licenses.bsd3;
}
