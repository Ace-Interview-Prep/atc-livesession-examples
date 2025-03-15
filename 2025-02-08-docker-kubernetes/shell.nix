{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
      cabal-install
      ghcid
    ]);
}


#{ pkgs ? import <nixpkgs> {} }:
#pkgs.haskellPackages.callPackage ./my-ml-service.nix {}

# shell.nix
# { pkgs ? import <nixpkgs> {} }:
#
# pkgs.mkShell {
#   buildInputs = [
#     pkgs.haskellPackages.ghc
#     pkgs.haskellPackages.cabal-install
#   ];
#   nativeBuildInputs = [
#     pkgs.haskellPackages.haskell-language-server
#   ];
#
#   shellHook = ''
#     cabal update
#   '';
# }
