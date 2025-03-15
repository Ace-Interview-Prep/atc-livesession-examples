{ mkDerivation, pkgs, base, bytestring, http-client, http-conduit
, http-types, lib, matrix, stm, wai, warp
}:
mkDerivation {
  pname = "my-ml-service";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableSystemDepends = [
    pkgs.zlib
    pkgs.gcc
    pkgs.gmp
    pkgs.libffi
  ];
  executableHaskellDepends = [
    base bytestring http-client http-conduit http-types matrix stm wai
    warp
  ];
  license = lib.licenses.mit;
  mainProgram = "my-ml-service";
}































# { pkgs ? import <nixpkgs> {} }:
# let
#   myPackage = pkgs.haskellPackages.callCabal2nix "my-ml-service" ./. {};
# in
# pkgs.stdenv.mkDerivation rec {
#   pname = "my-ml-service";
#   version = "1.0.0";

#   src = myPackage.src;

#   buildInputs = myPackage.buildInputs ++ [
#     pkgs.curl
#     pkgs.haskellPackages.ghc
#     pkgs.haskellPackages.cabal-install
#     pkgs.gcc
#     pkgs.gmp
#     pkgs.libffi
#   ];

#   buildPhase = ''
#     cabal update
#     cabal build
#   '';
# }












# default.nix
# { pkgs ? import <nixpkgs> {} }:

# pkgs.haskellPackages.developPackage {
#   name = "my-ml-service";
#   src = ./.;
#   version = "0.1.0.0";
#   isLibrary = false;
#   isExecutable = true;
#   executable = {
#     mainModule = "Main.hs";
#   };
#   buildInputs = [ pkgs.ghc pkgs.cabal-install ];
# }
