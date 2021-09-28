{ system ? builtins.currentSystem
}:
let 
  gitignoreSrc = builtins.fetchTarball {
    url = "https://github.com/hercules-ci/gitignore/archive/c4662e6.tar.gz";
    sha256 = "1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
  };
  inherit (import (gitignoreSrc) { }) gitignoreSource;
  reflexPlatformSrc = builtins.fetchGit { 
    url = "https://github.com/reflex-frp/reflex-platform.git";
    rev = "194e83f436f7d67489f553fb385546acf02ec998";
  };
  reflexPlatform = import reflexPlatformSrc { 
    inherit system;
  };
  project = reflexPlatform.project ({pkgs, ...}: {
    useWarp = true;
    withHoogle = false;
    packages = {
      reflex-stone = pkgs.lib.cleanSource (gitignoreSource ./.);
    };
    shells = {
      ghc = ["reflex-stone"];
      ghcjs = ["reflex-stone"];
    };
    overrides = self: super: let
      gf-udSrc = builtins.fetchGit {
        url = "https://github.com/GrammaticalFramework/gf-ud.git";
        rev = "4b0760e02f9efdb8fbdd47ed2258e9b89f15a14d";
      };
      aceSrc = builtins.fetchGit {
        url = "https://github.com/reflex-frp/reflex-dom-ace";
        rev = "f5a7f5999c20f2a27aeaa93f96f093bcc560b526";
      };
      baby-l4Src = builtins.fetchGit {
        url = "https://github.com/smucclaw/baby-l4.git";
        rev = "3619459461b52864e38e6eecf36f0c94603cbc84";
      };
      haskellPackages = (import <nixpkgs> {}).haskellPackages;
      xhtml_3000_2_1Src = haskellPackages.callHackage "xhtml" "3000.2.1" {};
      haskeline_0_7_3_1Src = haskellPackages.callHackage "haskeline" "0.7.3.1" {};
      terminfo_0_4_1_1Src = haskellPackages.callHackage "terminfo" "0.4.1.1" {};
    in
    {
      gf-ud = self.callCabal2nix "gf-ud" gf-udSrc {};
      reflex-dom-ace = self.callCabal2nix "reflex-dom-ace" aceSrc {};
      baby-l4 = self.callCabal2nix "baby-l4" baby-l4Src {};
      xhtml_3000_2_1 = xhtml_3000_2_1Src;
      haskeline_0_7_3_1 = haskeline_0_7_3_1Src;
      terminfo_0_4_1_1 = terminfo_0_4_1_1Src;
      lsp = null;
      lsp-types = null;
      lsp-test = null;
      sbv = null;
    };
  });
in {
  inherit project reflexPlatform;
}
