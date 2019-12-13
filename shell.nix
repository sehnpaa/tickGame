{ nixpkgs ? import (fetchTarball {
    url = https://github.com/NixOS/nixpkgs-channels/archive/971b731fc18c86569211a460ef62e1d8001799e9.tar.gz;
    sha256 = "1b8xjrrwb8fz92bcrqvfvfg7gwn40ss12by2ka4yclcxk5yylmw0";
  }) {} , compiler ? "ghc865", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, bytestring, gi-gdk, gi-gtk
      , gi-gtk-declarative, gi-gtk-declarative-app-simple, lens
      , ListZipper, megaparsec, mtl, pretty-simple, stdenv, tasty
      , tasty-hunit, tasty-quickcheck, text, validation, vector
      }:
      mkDerivation {
        pname = "tickGame";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryHaskellDepends = [
          base gi-gtk gi-gtk-declarative gi-gtk-declarative-app-simple lens
          ListZipper megaparsec mtl pretty-simple tasty tasty-hunit
          tasty-quickcheck text validation
        ];
        executableHaskellDepends = [
          async base bytestring gi-gdk gi-gtk gi-gtk-declarative
          gi-gtk-declarative-app-simple tasty tasty-hunit tasty-quickcheck
          text vector
        ];
        testHaskellDepends = [
          base lens ListZipper mtl tasty tasty-hunit tasty-quickcheck text
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
