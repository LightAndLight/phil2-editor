{ mkDerivation, base, bricki, containers, data-default, fetchgit
, microlens, microlens-th, reflex, reflex-host, stdenv, stm, text
, these, transformers, vty
}:
mkDerivation {
  pname = "bricki-reflex";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lspitzner/bricki-reflex";
    sha256 = "0qi2k14w41lqxbx9j05j7ivlllc7mqxkv3769dl5ws8d0svi6g7v";
    rev = "b97ccef05f10bfbdac5ff56e1167af9ef62c99b1";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bricki containers microlens reflex reflex-host stm these vty
  ];
  executableHaskellDepends = [
    base bricki containers data-default microlens microlens-th reflex
    reflex-host text transformers vty
  ];
  license = stdenv.lib.licenses.bsd3;
}
