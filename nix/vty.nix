{ mkDerivation, base, blaze-builder, bytestring, Cabal, containers
, deepseq, directory, filepath, hashable, HUnit, microlens
, microlens-mtl, microlens-th, mtl, parallel, parsec, QuickCheck
, quickcheck-assertions, random, smallcheck, stdenv, stm, string-qq
, terminfo, test-framework, test-framework-hunit
, test-framework-smallcheck, text, transformers, unix, utf8-string
, vector
}:
mkDerivation {
  pname = "vty";
  version = "5.18.1";
  sha256 = "1ca772dc231df82334fc2b2976453330d32e84a6a28bf581548eb898a32660eb";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base blaze-builder bytestring containers deepseq directory filepath
    hashable microlens microlens-mtl microlens-th mtl parallel parsec
    stm terminfo text transformers unix utf8-string vector
  ];
  executableHaskellDepends = [
    base containers microlens microlens-mtl mtl
  ];
  testHaskellDepends = [
    base blaze-builder bytestring Cabal containers deepseq HUnit
    microlens microlens-mtl mtl QuickCheck quickcheck-assertions random
    smallcheck stm string-qq terminfo test-framework
    test-framework-hunit test-framework-smallcheck text unix
    utf8-string vector
  ];
  homepage = "https://github.com/jtdaugherty/vty";
  description = "A simple terminal UI library";
  license = stdenv.lib.licenses.bsd3;
}
