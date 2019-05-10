{ nixpkgs ? import (fetchTarball {
    url = https://github.com/NixOS/nixpkgs-channels/archive/7defc47944fe6d1da4c3a08c60c8332ca660a680.tar.gz;
    sha256 = "100mh7ir6cca2yjv89nr4wkns3567v9kz6dnc6ysyvd579fw03a7";
  }) {} , compiler ? "ghc864", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, gi-gtk, gi-gtk-declarative
      , gi-gtk-declarative-app-simple, lens, stdenv, tasty, tasty-hunit
      , tasty-quickcheck, text, vector
      }:
      mkDerivation {
        pname = "tickGame";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryHaskellDepends = [
          base gi-gtk gi-gtk-declarative gi-gtk-declarative-app-simple lens
          tasty tasty-hunit tasty-quickcheck text
        ];
        executableHaskellDepends = [
          base gi-gtk gi-gtk-declarative gi-gtk-declarative-app-simple tasty
          tasty-hunit tasty-quickcheck text vector
        ];
        testHaskellDepends = [
          base lens tasty tasty-hunit tasty-quickcheck
        ];
        doHaddock = false;
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
