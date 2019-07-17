{ stdenv, pkgs, nodejs ? pkgs."nodejs-8_x" }:

let
  # node-packages*.nix generated via:
  #
  # % node2nix --input node-packages.json \
  #            --output node-packages-generated.nix \
  #            --composition node-packages.nix \
  #            --node-env node-env.nix
  #
  nodePackages = import ./node-packages.nix {
    inherit pkgs;
    inherit (stdenv.hostPlatform) system;
  };
in rec {
  antora = nodePackages."@antora/cli".override {
    buildInputs = [ pkgs.makeWrapper ];
    postInstall = ''
      wrapProgram $out/bin/antora --set NODE_PATH "${antora-gen}/lib/node_modules"
    '';
  };
  antora-gen = nodePackages."@antora/site-generator-default";
}
