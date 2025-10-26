{ mkDerivation, aeson, base, base-compat, base64-bytestring
, bytestring, constraints, containers, directory, exceptions
, filepath, hspec, hspec-discover, hspec-wai, http-api-data
, http-media, http-types, lib, monad-control, mtl, network
, resourcet, safe, servant, should-not-typecheck, sop-core, tagged
, temporary, text, transformers, transformers-base, wai
, wai-app-static, wai-extra, warp, word8
}:
mkDerivation {
  pname = "servant-server";
  version = "0.20.3.0";
  sha256 = "30560af5d2597ae361711de8302617de3bfb3e01f10180ff48a331bbe8e49915";
  revision = "1";
  editedCabalFile = "1z2h1gmxphwd76chyah405ww4ciyxq7rvggghr6lh0z1m3p2k90h";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base64-bytestring bytestring constraints containers exceptions
    filepath http-api-data http-media http-types monad-control mtl
    network resourcet servant sop-core tagged text transformers
    transformers-base wai wai-app-static word8
  ];
  executableHaskellDepends = [
    aeson base base-compat text wai warp
  ];
  testHaskellDepends = [
    aeson base base-compat base64-bytestring bytestring directory hspec
    hspec-wai http-types mtl resourcet safe servant
    should-not-typecheck temporary text wai wai-extra
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://docs.servant.dev/";
  description = "A family of combinators for defining webservices APIs and serving them";
  license = lib.licenses.bsd3;
  mainProgram = "greet";
}
