{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  haskellLib = pkgs.haskell.lib;
in
mkShell {
  buildInputs =
    let
      nix-thunkSrc = pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "nix-thunk";
        rev = "8fe6f2de2579ea3f17df2127f6b9f49db1be189f";
        sha256 = "14l2k6wipam33696v3dr3chysxhqcy0j7hxfr10c0bxd1pxv7s8b";
      };
      nix-thunk = import nix-thunkSrc {};

      GPipe-Src = nix-thunk.thunkSource ./thunks/GPipe-Core;

      myHaskell = pkgs.haskell.packages.ghc966.override {
        overrides = self: super: {
          GPipe = haskellLib.doJailbreak (self.callCabal2nix "GPipe" (GPipe-Src + "/GPipe-Core") {});
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
