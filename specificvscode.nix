{ nixpkgs ? import (fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/f23870a45b5844d2e8f49afc8a9f1a12dd2a8b66.tar.gz;
  }) {}}:


nixpkgs.mkShell {
  buildInputs = [ nixpkgs.vscode ];
}
