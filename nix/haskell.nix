############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, stdenv
, pkgs
, haskell-nix
, buildPackages
}:

let
  pkgSet = haskell-nix.stackProject {
    src = ../.;
    # This turns the output into a fixed-output derivation, which speeds things
    # up, but means we need to invalidate this hash when we change things.
    stack-sha256 = "0356fsjnb5lgzn7xw2wqphy97c2yxr3bdfd55a6kd84z9p475vcx";
    modules = [
        {
          nonReinstallablePkgs =
            [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
              "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
              "ghc-boot"
              "ghc" "Cabal" "Win32" "array" "binary" "bytestring" "containers"
              "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
              "ghci" "haskeline"
              "hpc"
              "mtl" "parsec" "process" "text" "time" "transformers"
              "unix" "xhtml"
              "stm" "terminfo"
            ];
          packages.marlowe.doHaddock = false;
          # HACK to get z3 on the path for these tests
          packages.marlowe-hspec.components.tests.marlowe-hspec-test.preCheck = ''
            PATH=${lib.makeBinPath [ pkgs.z3 ]}:$PATH
          '';
        }
     ];
    pkg-def-extras = [
      # Workaround for https://github.com/input-output-hk/haskell.nix/issues/214
      (hackage: {
        packages = {
          "hsc2hs" = (((hackage.hsc2hs)."0.68.4").revisions).default;
        };
      })
    ];
  };

in
  pkgSet
