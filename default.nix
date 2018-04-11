(import ./reflex-platform {}).project ({ pkgs, ...}: {
  packages = { phil2-editor = ./.; };
  shells = { ghc = ["phil2-editor"]; };
  overrides = self: super: {
    bricki = self.callPackage ./nix/bricki.nix {};
    bricki-reflex = self.callPackage ./nix/bricki-reflex.nix {};
    reflex-host = self.callPackage ./nix/reflex-host.nix {};
    vty = pkgs.haskell.lib.dontCheck (self.callPackage ./nix/vty.nix {});
  };
})
