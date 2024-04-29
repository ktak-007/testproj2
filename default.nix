{ sources ? import ./nix/sources.nix
}:
let pkgs = import sources.nixpkgs {};
in
{ testproj2 = pkgs.haskellPackages.developPackage { root = ./.; };
#{ testproj2 = pkgs.haskellPackages.callCabal2nix "testproj2" ./. {};
}
