{ system ? builtins.currentSystem
}:
let 
  name = "reflex-stone";
  p = import ./project.nix { inherit system; };
  pkgs = p.reflexPlatform.nixpkgs;
  app = pkgs.lib.getAttr name p.project.ghcjs;
in
 app
