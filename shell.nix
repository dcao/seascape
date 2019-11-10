{ pkgs ? import <nixpkgs> {} }:

let
  hsPkgs = import ./default.nix {};
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in
  hsPkgs.shellFor {
    # Include only the *local* packages of your project.
    packages = ps: with ps; [
      seascape
    ];

    # Builds a Hoogle documentation index of all dependencies,
    # and provides a "hoogle" command to search the index.
    withHoogle = true;

    # You might want some extra tools in the shell (optional).
    buildInputs = [ pkgs.nodejs pkgs.haskellPackages.cabal-install (all-hies.selection { selector = p: { inherit (p) ghc865; }; }) ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  }
