let pkgs = import <nixpkgs> {};
in { testproj2 = pkgs.haskellPackages.callPackage ./testproj2.nix {}; }
