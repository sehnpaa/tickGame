{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc844", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, gi-gtk, gi-gtk-declarative
      , gi-gtk-declarative-app-simple, microlens-platform, stdenv, tasty
      , tasty-hunit, tasty-quickcheck, text, vector
      }:
      mkDerivation {
        pname = "tickGame";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryHaskellDepends = [
          base gi-gtk gi-gtk-declarative gi-gtk-declarative-app-simple
          microlens-platform tasty tasty-hunit tasty-quickcheck text vector
        ];
        executableHaskellDepends = [
          base gi-gtk gi-gtk-declarative gi-gtk-declarative-app-simple
          microlens-platform tasty tasty-hunit tasty-quickcheck text vector
        ];
        testHaskellDepends = [
          base tasty tasty-hunit tasty-quickcheck vector
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
