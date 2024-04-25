{ nixpkgs ? import <nixpkgs> {}
, sources ? import ./nix/sources.nix
}:
let pkgs = import sources.nixpkgs {};
in nixpkgs.mkShell {
  packages = with pkgs; [ cabal-install cabal2nix ];
  inputsFrom = [ (import ./default.nix {}).testproj2.env ];
}
