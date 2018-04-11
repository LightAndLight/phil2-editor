{ mkDerivation, base, deriving-compat, equivalence, fetchgit, lens
, mtl, stdenv
}:
mkDerivation {
  pname = "unification";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lightandlight/unification";
    sha256 = "0gznlrjq0709qlymzs1p8gcja5n4ba2b903bqbdha73d2jnr9d0c";
    rev = "21c94b88613a018ec023327c4a9b146c223cd1e3";
  };
  libraryHaskellDepends = [
    base deriving-compat equivalence lens mtl
  ];
  license = stdenv.lib.licenses.bsd3;
}
