{ mkDerivation, attoparsec, base, bytestring, case-insensitive
, criterion, double-conversion, lib, QuickCheck, tasty, tasty-quickcheck
, text, transformers
}:
mkDerivation {
  pname = "bytestring-conversion";
  version = "0.3.2";
  sha256 = "0ls1jqf4r2hk0mcxmlviw6vgs0cn1db99w2fggsg6x39pi31rk8c";
  libraryHaskellDepends = [
    attoparsec base bytestring case-insensitive double-conversion text
  ];
  testHaskellDepends = [
    base bytestring QuickCheck tasty tasty-quickcheck
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion text transformers
  ];
  description = "Type-classes to convert values to and from ByteString";
  license = lib.licenses.mpl20;
}
