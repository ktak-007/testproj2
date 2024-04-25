{ nixpkgs ? import <nixpkgs> {}
, sources ? import ./nix/sources.nix
}:
let pkgs = import sources.nixpkgs {};
in nixpkgs.mkShell {
  packages = with pkgs; [ cabal-install ];
  inputsFrom = [ (import ./default.nix {}).testproj2.env ];
}
