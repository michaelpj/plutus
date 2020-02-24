# our packages overlay
pkgs: _: with pkgs; {
  plutusHaskellPackages = import ./haskell.nix {
    inherit
      lib
      stdenv
      pkgs
      haskell-nix
      buildPackages
      ;
  };
}