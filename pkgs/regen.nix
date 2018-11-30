let
  localLib = import ../lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, iohkPkgs ? import ../. { inherit config system; }
, pkgs ? iohkPkgs.pkgs
, nixpkgsPath ? localLib.iohkNix.nixpkgs

# Update this if you need a package version recently uploaded to hackage.
# Any timestamp works.
, hackageSnapshot ? "2018-11-08T09:58:14Z"
}:

with pkgs;

stdenvNoCC.mkDerivation {
  name = "generated-stack-packages";
  src =  (pkgs.lib.sourceFilesBySuffices (localLib.iohkNix.cleanSourceHaskell ../.) [".yaml" ".project" ".cabal" ]);
  buildInputs = [ nixStable nix-prefetch-scripts coreutils git findutils cabal2nix glibcLocales stack cabal-install iohkPkgs.stack2nix haskell.compiler.ghc843 ];
  buildPhase = ''
    set -euo pipefail
    export HOME=$(pwd)
    export NIX_PATH=nixpkgs=${nixpkgsPath}

    echo "Using hackage snapshot from ${hackageSnapshot}"

    # Ensure that nix 1.11 (used by cabal2nix) works in multi-user mode.
    if [ ! -w /nix/store ]; then
        export NIX_REMOTE=''${NIX_REMOTE:-daemon}
    fi

    # Generate package set
    mkdir $out
    stack2nix --verbose --platform x86_64-linux --hackage-snapshot "${hackageSnapshot}" -j8 --test --bench --no-indent . > $out/default.nix
  '';
  outputHashMode = "recursive";
  outputHashAlgo = "sha256";
  outputHash = "10h0qs7svw0cqjkyxs8z6s3qraa8ga920zfrr59rdlanbwg4klly";
}
