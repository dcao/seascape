{ pkgs ? import (builtins.fetchTarball "https://github.com/input-output-hk/nixpkgs/archive/3d623a406cec9052ae0a16a79ce3ce9de11236bb.tar.gz") (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz))
, haskellCompiler ? "ghc865"
}:
  pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    ghc = pkgs.haskell.compiler.${haskellCompiler};
  }
