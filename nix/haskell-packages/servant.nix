{ mkDerivation, aeson, attoparsec, base, base-compat, bifunctors
, bytestring, case-insensitive, constraints, deepseq, hspec
, hspec-discover, http-api-data, http-media, http-types, lib
, mmorph, mtl, network-uri, QuickCheck, quickcheck-instances
, singleton-bool, sop-core, string-conversions, tagged, text
, transformers, vault
}:
mkDerivation {
  pname = "servant";
  version = "0.20.1";
  sha256 = "94f773f9bdff67eb2d7aec9c67f80e8af79d531ba6f52fad2e142d8ce4551be9";
  revision = "2";
  editedCabalFile = "137yfr7mxfx2r3pkdfwsxv7xxch5l20yirj82186djyg36q5021z";
  libraryHaskellDepends = [
    aeson attoparsec base base-compat bifunctors bytestring
    case-insensitive constraints deepseq http-api-data http-media
    http-types mmorph mtl network-uri QuickCheck singleton-bool
    sop-core string-conversions tagged text transformers vault
  ];
  testHaskellDepends = [
    aeson base base-compat bytestring hspec http-media mtl QuickCheck
    quickcheck-instances string-conversions text transformers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://docs.servant.dev/";
  description = "A family of combinators for defining webservices APIs";
  license = lib.licenses.bsd3;
}
