{ mkDerivation, base, bricki, containers, data-default, fetchgit
, microlens, microlens-th, reflex, reflex-host, stdenv, stm, text
, these, transformers, vty
}:
mkDerivation {
  pname = "bricki-reflex";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lightandlight/bricki-reflex";
    sha256 = "1d2kqkka2jv1mq1hz713ws4p78iiwjr6jbgkbwj7vy7s8s6rhb2c";
    rev = "605f14a4e70261d5045c6e7cc8784e6d14170e4f";
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
