{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  haskellLib = pkgs.haskell.lib;
in
mkShell {
  buildInputs =
    let
      myHaskell = pkgs.haskell.packages.ghc966.override {
        overrides = self: super: {
          GPipe = haskellLib.doJailbreak super.GPipe;
          transformers = self.callHackage "transformers" "0.5.6.2" {};
          # GPipe-GLFW = haskellLib.doJailbreak super.GPipe-GLFW;
        };
      };
    in
    [
    pkgs.SDL2
    pkgs.pkg-config
    pkgs.libGL
    pkgs.libGLU
    pkgs.haskellPackages.cabal-install
    (
      myHaskell.ghcWithPackages (haskellPkgs: [
        haskellPkgs.GPipe-GLFW
        haskellPkgs.hashtables
        haskellPkgs.linear
        haskellPkgs.transformers
      ])
    )
  ];
}



# src/Graphics/GPipe/Internal/Shader.hs:37:1: error:
#     Could not load module ‘Control.Monad.Trans.List’
#     It is a member of the hidden package ‘transformers-0.5.6.2’.
#     Perhaps you need to add ‘transformers’ to the build-depends in your .cabal file.
#     Use -v (or `:set -v` in ghci) to see a list of the files searched for.
#    |
# 37 | import Control.Monad.Trans.List (ListT(..))
#    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# [21 of 28] Compiling Graphics.GPipe.Orphans ( src/Graphics/GPipe/Orphans.hs, dist/build/Graphics/GPipe/Orphans.o, dist/build/Graphics/GPipe/Orphans.dyn_o )
# error: builder for '/nix/store/g650737p0gq3znk49hj80h4jg4db5wfw-GPipe-2.2.5.drv' failed with exit code 1
# error: 1 dependencies of derivation '/nix/store/rihw2nvpq1r9wd5rnvsz9h90fhxr6k6q-ghc-9.6.6-with-packages.drv' failed to build
# rhemsuda@STARFORGEPC:~/code/orgs/ace/examples/atc-livesession-examples/2025-07-12-pong$
