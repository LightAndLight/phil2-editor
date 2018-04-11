{ mkDerivation, base, bricki, containers, data-default, fetchgit
, microlens, microlens-th, reflex, reflex-host, stdenv, stm, text
, these, transformers, vty
}:
mkDerivation {
  pname = "bricki-reflex";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lightandlight/bricki-reflex";
    sha256 = "0pcbp64mspxg6k1bwr8p82g805dzlh8w92f3pnfdbm2sygv9m3si";
    rev = "1a6705c9f93f3b15052b8b27c8901dfbc71e0919";
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
