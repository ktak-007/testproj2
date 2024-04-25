{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc94" }:
{ testproj2 = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./testproj2.nix { }; }
