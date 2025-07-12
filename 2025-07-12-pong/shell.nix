{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    pkgs.SDL2
    pkgs.pkg-config
    pkgs.libGL
    pkgs.libGLU
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.cabal-install
  ];
}
