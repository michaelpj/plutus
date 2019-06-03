{ system ? builtins.currentSystem, config ? {} }:
let
  sources = import ./nix/sources.nix;
  iohkNix = import sources.iohk-nix { inherit config system; };

  # nixpkgs can be overridden for debugging purposes by setting
  # NIX_PATH=custom_nixpkgs=/path/to/nixpkgs
  pkgs = iohkNix.pkgs;
  nixpkgs = iohkNix.nixpkgs;
  lib = pkgs.lib;
  getPackages = iohkNix.getPackages;

  # List of all plutus pkgs. This is used for `isPlutus` filter and `mapTestOn`
  plutusPkgList = [
    "language-plutus-core"
    "plutus-contract-exe"
    "plutus-core-interpreter"
    "plutus-playground-server"
    "plutus-playground-lib"
    "plutus-playground-client"
    "plutus-server-invoker"
    "plutus-exe"
    "plutus-ir"
    "plutus-tx"
    "plutus-tutorial"
    "plutus-use-cases"
    "interpreter"
    "marlowe"
    "meadow"
    "plutus-wallet-api"
    "plutus-emulator"
  ];

  plutusHaskellPkgList = lib.filter (v: v != "plutus-playground-client" && v != "plutus-server-invoker") plutusPkgList;

  isPlutus = name: builtins.elem name plutusPkgList;

  regeneratePackages = iohkNix.stack2nix.regeneratePackages { hackageSnapshot = "2019-05-28T09:58:14Z"; };

  comp = f: g: (v: f(g v));
in lib // {
  inherit 
  getPackages 
  iohkNix 
  isPlutus 
  plutusHaskellPkgList 
  plutusPkgList 
  regeneratePackages 
  nixpkgs 
  pkgs
  comp;
}
