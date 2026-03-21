{ mkDerivation, aeson, async, base, blaze-builder, bytestring
, case-insensitive, containers, cookie, directory, doctest
, exceptions, hspec, hspec-discover, hspec-wai, http-api-data
, http-client, http-types, lib, lifted-base, lucid, monad-control
, mtl, network, random, regex-compat, resourcet, stm, text, time
, transformers, transformers-base, unliftio, unordered-containers
, wai, wai-extra, warp, weigh
}:
mkDerivation {
  pname = "scotty";
  version = "0.30";
  sha256 = "df90a8842400708e1314caaf0a7f0a026e214f3d5f98b7722edecc2e14fa4c4b";
  libraryHaskellDepends = [
    aeson base blaze-builder bytestring case-insensitive containers
    cookie exceptions http-api-data http-types monad-control mtl
    network random regex-compat resourcet stm text time transformers
    transformers-base unliftio unordered-containers wai wai-extra warp
  ];
  testHaskellDepends = [
    async base bytestring directory doctest hspec hspec-wai
    http-api-data http-client http-types lifted-base network text time
    wai wai-extra
  ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [
    base bytestring lucid mtl resourcet text transformers weigh
  ];
  homepage = "https://github.com/scotty-web/scotty";
  description = "Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp";
  license = lib.licenses.bsd3;
}
