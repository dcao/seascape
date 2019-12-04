# https://github.com/input-output-hk/haskell.nix/blob/79c2c631c3938093e7a7704bee3c7e094ee198e1/test/ghc-options/cabal.nix
let hn = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/00d61f7d3f8fd498a6ab66ff3188d1671bc14ac5.tar.gz);
in
  { pkgs ? import (builtins.fetchTarball "https://github.com/input-output-hk/nixpkgs/archive/a8f81dc037a5977414a356dd068f2621b3c89b60.tar.gz") {
      config = hn.config;
      overlays = hn.overlays ++ [ (self: super: {
        icuuc = self.icu;
        icui18n = self.icu;
        icudata = self.icu;
      }) ];
    }
  , haskellCompiler ? "ghc865"
  }:
  pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    ghc = pkgs.buildPackages.haskell-nix.compiler.${haskellCompiler};
    modules = [ {
      packages.seascape.enableExecutableProfiling = true;
      enableLibraryProfiling = true;
    } ];
  }
