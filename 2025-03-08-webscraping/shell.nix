{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  # scrappySrc = n.thunkSource ./deps/scrappy; 
  # scrappy = pkgs.haskell.lib.overrideCabal (callCabal2nix "scrappy" scrappySrc {}) {
  #   librarySystemDepends = [ nodejs ];
  # };
  nix-thunk = pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "nix-thunk";
    rev = "8fe6f2de2579ea3f17df2127f6b9f49db1be189f";
    sha256 = "14l2k6wipam33696v3dr3chysxhqcy0j7hxfr10c0bxd1pxv7s8b";
  };
  n = import nix-thunk {};
  scrappy-cores = n.thunkSource ./dep/scrappy-core;
  scrappy-core = pkgs.haskellPackages.callCabal2nix "scrappy-core" scrappy-cores {};

  
  f = { mkDerivation, base, bytestring, http-client
      , http-client-tls, lib, text
      }:
      mkDerivation {
        pname = "scrape";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bytestring http-client http-client-tls scrappy-core text
        ];
        license = lib.licenses.mit;
        mainProgram = "scrape";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in
pkgs.mkShell {
  buildInputs = [ pkgs.cabal-install ];
  inputsFrom = [ (if pkgs.lib.inNixShell then drv.env else drv) ];
} 

