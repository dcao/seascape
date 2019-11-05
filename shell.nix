with import <nixpkgs> {};
let
  overlay = (self: super:
    let
      myOverride = {};
    in {
      # Add an override for each required python version. 
      # There’s currently no way to add a package that’s automatically picked up by 
      # all python versions, besides editing python-packages.nix
      python2 = super.python2.override myOverride;
      python3 = super.python3.override myOverride;
      python37 = super.python37.override myOverride;
    }
  );

  pkgs = import <nixpkgs> { overlays = [ overlay ]; };
in
 (pkgs.python37.withPackages (ps: with ps; [
   flask pandas selenium natsort statsmodels scikitlearn gunicorn requests beautifulsoup4 ipython
 ])).env
