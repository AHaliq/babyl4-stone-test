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
  project = reflexPlatform.project ({ pkgs, ... }: {
    useWarp = true;
    withHoogle = false;
    packages = {
      reflex-stone = pkgs.lib.cleanSource (gitignoreSource ./.);
      baby-l4-core = pkgs.lib.cleanSource (gitignoreSource ./baby-l4/baby-l4-core);
    };
    shells = {
      ghc = [ "reflex-stone" "baby-l4-core" ];
      ghcjs = [ "reflex-stone" ];
    };
    overrides = self: super:
      let
        #gf-udSrc = builtins.fetchGit {
        #  url = "https://github.com/GrammaticalFramework/gf-ud.git";
        #  rev = "bd319852e7319e7c905a6807d9d8f9e273aec6d1";
        #};
        ace-src = pkgs.fetchFromGitHub {
          owner = "SlimTim10";
          repo = "reflex-dom-ace";
          rev = "5d086c871892b9ebf7c66ad2d4d4f84023bea9b2";
          sha256 = "0vizlf691s1rbkilhf23c79vkb6pns2x4b6b94szn9wd8mjizxyc";
        };
        #xhtml_3000_2_1Src = haskellPackages.callHackage "xhtml" "3000.2.1" {};
        #haskeline_0_7_3_1Src = haskellPackages.callHackage "haskeline" "0.7.3.1" {};
        #terminfo_0_4_1_1Src = haskellPackages.callHackage "terminfo" "0.4.1.1" {};
      in
      {
        #gf-ud = self.callCabal2nix "gf-ud" gf-udSrc {};
        #self.callHackage "" src {};
        reflex-dom-ace = self.callCabal2nix "reflex-dom-ace" ace-src { };
        #xhtml_3000_2_1 = xhtml_3000_2_1Src;
        #haskeline_0_7_3_1 = haskeline_0_7_3_1Src;
        #terminfo_0_4_1_1 = terminfo_0_4_1_1Src;
        lsp = null;
        lsp-types = null;
        lsp-test = null;
        sbv = null;
      };
  });
in
{
  inherit project reflexPlatform;
}
