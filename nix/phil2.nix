{ mkDerivation, base, bytestring, containers, deriving-compat
, fetchgit, lens, mtl, parsers, stdenv, trifecta, unification
}:
mkDerivation {
  pname = "phil2";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lightandlight/phil2";
    sha256 = "1jhf53fkkq3ljha6h350kd929nfixkp74ld25dar2h07vhlavavi";
    rev = "176674c36e567b21906a059d0a36b7095549331d";
  };
  libraryHaskellDepends = [
    base bytestring containers deriving-compat lens mtl parsers
    trifecta unification
  ];
  license = stdenv.lib.licenses.bsd3;
}
