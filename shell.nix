{ pkgs ? import <nixpkgs> {
  overlays = [
    (self: super: {
      icuuc = self.icu;
      icui18n = self.icu;
      icudata = self.icu;
    })
  ];
} }:

let
  hsPkgs = import ./default.nix {};
in
  hsPkgs.shellFor {
    # Include only the *local* packages of your project.
    packages = ps: with ps; [
      seascape
    ];

    # You might want some extra tools in the shell (optional).
    buildInputs = [ pkgs.nodejs pkgs.selenium-server-standalone pkgs.cabal-install ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  }
