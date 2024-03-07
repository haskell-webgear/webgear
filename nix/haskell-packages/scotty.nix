{ mkDerivation, aeson, async, base, blaze-builder, bytestring
, case-insensitive, cookie, data-default-class, directory, doctest
, exceptions, hspec, hspec-discover, hspec-wai, http-client
, http-types, lib, lifted-base, lucid, monad-control, mtl, network
, regex-compat, stm, text, time, transformers, transformers-base
, transformers-compat, unliftio, wai, wai-extra, warp, weigh
}:
mkDerivation {
  pname = "scotty";
  version = "0.21";
  sha256 = "24fa7282f742fa4d98340a40777e6c6c37e0902ed7c7c5879295f51cf953dee2";
  revision = "1";
  editedCabalFile = "0cg0s728112n8xy966cwmbvvjn2qnd6magibj9q3cxn41yinlzyi";
  libraryHaskellDepends = [
    aeson base blaze-builder bytestring case-insensitive cookie
    data-default-class exceptions http-types monad-control mtl network
    regex-compat stm text time transformers transformers-base
    transformers-compat unliftio wai wai-extra warp
  ];
  testHaskellDepends = [
    async base bytestring directory doctest hspec hspec-wai http-client
    http-types lifted-base network text wai
  ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [
    base bytestring lucid mtl text transformers weigh
  ];
  homepage = "https://github.com/scotty-web/scotty";
  description = "Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp";
  license = lib.licenses.bsd3;
}
