{ mkDerivation, base, containers, contravariant, data-clist
, deepseq, dlist, fetchgit, microlens, microlens-mtl, microlens-th
, stdenv, stm, template-haskell, text, text-zipper, transformers
, vector, vty, word-wrap
}:
mkDerivation {
  pname = "bricki";
  version = "0.28.0.100";
  src = fetchgit {
    url = "https://github.com/lspitzner/bricki";
    sha256 = "0bhnm7nn93qy04wpym6cqqh6312lvq9hf5bdyc77nl6pn4zg32fy";
    rev = "2098d402d9426b6b099ed0beb384e082c28c0327";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers contravariant data-clist deepseq dlist microlens
    microlens-mtl microlens-th stm template-haskell text text-zipper
    transformers vector vty word-wrap
  ];
  executableHaskellDepends = [
    base microlens microlens-th text text-zipper vector vty word-wrap
  ];
  homepage = "https://github.com/lspitzner/bricki/";
  description = "A declarative terminal user interface library";
  license = stdenv.lib.licenses.bsd3;
}
