{ mkDerivation, aeson, async, base, blaze-builder, bytestring
, case-insensitive, cookie, data-default-class, directory, doctest
, exceptions, hspec, hspec-discover, hspec-wai, http-client
, http-types, lib, lifted-base, lucid, monad-control, mtl, network
, regex-compat, resourcet, stm, text, time, transformers
, transformers-base, unliftio, wai, wai-extra, warp, weigh
}:
mkDerivation {
  pname = "scotty";
  version = "0.22";
  sha256 = "ceb998c12502cd639c8aa59c5b5ea8f2651198e62f2f86cf18b8b5a087b4b81c";
  revision = "2";
  editedCabalFile = "1m3qvb5q6yigw6ijxnp6h66rmyqg54619hb240s7cqc9qjrrkixk";
  libraryHaskellDepends = [
    aeson base blaze-builder bytestring case-insensitive cookie
    data-default-class exceptions http-types monad-control mtl network
    regex-compat resourcet stm text time transformers transformers-base
    unliftio wai wai-extra warp
  ];
  testHaskellDepends = [
    async base bytestring directory doctest hspec hspec-wai http-client
    http-types lifted-base network text time wai wai-extra
  ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [
    base bytestring lucid mtl resourcet text transformers weigh
  ];
  homepage = "https://github.com/scotty-web/scotty";
  description = "Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp";
  license = lib.licenses.bsd3;
}
