{ mkDerivation, base, bytestring, containers, deriving-compat
, fetchgit, lens, mtl, parsers, stdenv, trifecta, unification
}:
mkDerivation {
  pname = "phil2";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lightandlight/phil2";
    sha256 = "1srf44pcr06sy0qvlip7ifiwmzvni41lxcdz2xdmsmvyxn4bq703";
    rev = "52ae335b70a88896726b3e096cda6f0083cc1573";
  };
  libraryHaskellDepends = [
    base bytestring containers deriving-compat lens mtl parsers
    trifecta unification
  ];
  license = stdenv.lib.licenses.bsd3;
}
