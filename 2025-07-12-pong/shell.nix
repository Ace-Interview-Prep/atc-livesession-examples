{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  haskellLib = pkgs.haskell.lib;
in
mkShell {
  buildInputs =
    let
      GPipe-Src = pkgs.fetchFromGitHub {
        owner = "tobbebex";
        repo = "GPipe-Core";
        rev = "4f512f1ea6e6c32bbefaae1be38a68508337b1fa";
        sha256 = "sha256-zk3patORx/vZbgnvDgsLnRnO07L8Ot2cMCCsGmnGzzg=";
      };

      myHaskell = pkgs.haskell.packages.ghc966.override {
        overrides = self: super: {
          GPipe = let
            GPipe1 = haskellLib.doJailbreak (self.callCabal2nix "GPipe" (GPipe-Src + "/GPipe-Core") {});
            transformers1 = (self.callHackage "transformers" "0.5.6.2" {});
            hashtables1 = haskellLib.doJailbreak (self.callHackage "hashtables" "1.2.4.2" {});
            linear1 = haskellLib.doJailbreak (self.callHackage "linear" "1.18" {});
          in
            haskellLib.overrideCabal super.GPipe {
              buildDepends = [
                transformers1
                hashtables1
                linear1
              ];
            };
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
