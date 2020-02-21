# our packages overlay
pkgs: _: with pkgs; {
  plutusHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      pkgs
      haskell-nix
      buildPackages
      ;
  };
}