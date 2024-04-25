{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc94" }:
(import ./default.nix { inherit nixpkgs compiler; }).testproj2.env
