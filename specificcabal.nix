{ nixpkgs ? import (fetchTarball {
    url = https://github.com/NixOS/nixpkgs-channels/archive/971b731fc18c86569211a460ef62e1d8001799e9.tar.gz;
  }) {}}:

nixpkgs.mkShell {
  buildInputs = [ nixpkgs.cabal-install ];
}
