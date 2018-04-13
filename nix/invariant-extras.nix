{ mkDerivation, base, contravariant, fetchgit, generics-eot
, invariant, semigroupoids, stdenv, transformers
}:
mkDerivation {
  pname = "invariant-extras";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/qfpl/invariant-extras";
    sha256 = "1v0awswl2d7ybbfhmqyhmj7c9810lfk2z2gsm1lk1fypndlajyj6";
    rev = "0a0b7c03fc6f827254ac7607c19990fba45da626";
  };
  postUnpack = "sourceRoot+=/invariant-extras; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base contravariant generics-eot invariant semigroupoids
    transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
