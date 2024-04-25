{ sources ? import ./nix/sources.nix
}:
let pkgs = import sources.nixpkgs {};
in
{ testproj2 = pkgs.haskellPackages.callPackage ./testproj2.nix { };
}
