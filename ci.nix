let 
  packageSet = import ./default.nix {};
  pkgs = packageSet.pkgs;
in { 
  plutusPackages = pkgs.recurseIntoAttrs packageSet.local-packages-new;
}
