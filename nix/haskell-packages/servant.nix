{ mkDerivation, aeson, attoparsec, base, bifunctors, bytestring
, case-insensitive, constraints, containers, deepseq, hspec
, hspec-discover, http-api-data, http-media, http-types, lib
, mmorph, mtl, network-uri, QuickCheck, quickcheck-instances
, singleton-bool, sop-core, text, transformers, vault
}:
mkDerivation {
  pname = "servant";
  version = "0.20.2";
  sha256 = "6a39e279d34f42b20eace9b5296fa8dcfd2116ed7391d99f58ba005bb3f45365";
  revision = "1";
  editedCabalFile = "17n769vwyyc5hshm71r33ksvn26qcz19017wl9p8xj4igav790pa";
  libraryHaskellDepends = [
    aeson attoparsec base bifunctors bytestring case-insensitive
    constraints containers deepseq http-api-data http-media http-types
    mmorph mtl network-uri QuickCheck singleton-bool sop-core text
    transformers vault
  ];
  testHaskellDepends = [
    aeson base bytestring hspec http-media mtl QuickCheck
    quickcheck-instances text
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://docs.servant.dev/";
  description = "A family of combinators for defining webservices APIs";
  license = lib.licenses.bsd3;
}
