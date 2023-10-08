{ mkDerivation, aeson, async, base, blaze-builder, bytestring
, case-insensitive, cookie, data-default-class, directory
, exceptions, hspec, hspec-discover, hspec-wai, http-types, lib
, lifted-base, lucid, monad-control, mtl, network, regex-compat
, stm, text, time, transformers, transformers-base
, transformers-compat, unliftio, wai, wai-extra, warp, weigh
}:
mkDerivation {
  pname = "scotty";
  version = "0.20.1";
  sha256 = "6d9a886c49aaef9b46f06f5aea61a57113d32212ddc7a9ef1921b68f8e9ce09c";
  libraryHaskellDepends = [
    aeson base blaze-builder bytestring case-insensitive cookie
    data-default-class exceptions http-types monad-control mtl network
    regex-compat stm text time transformers transformers-base
    transformers-compat unliftio wai wai-extra warp
  ];
  testHaskellDepends = [
    async base bytestring directory hspec hspec-wai http-types
    lifted-base network text wai
  ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [
    base bytestring lucid mtl text transformers weigh
  ];
  homepage = "https://github.com/scotty-web/scotty";
  description = "Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp";
  license = lib.licenses.bsd3;
}
