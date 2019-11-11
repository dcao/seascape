{ pkgs ? import <nixpkgs> {} }:

let
  hsPkgs = import ./default.nix {};
in
  hsPkgs.shellFor {
    # Include only the *local* packages of your project.
    packages = ps: with ps; [
      seascape
    ];

    # You might want some extra tools in the shell (optional).
    buildInputs = [ pkgs.nodejs ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  }
