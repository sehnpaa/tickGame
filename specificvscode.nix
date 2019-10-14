{ nixpkgs ? import (fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/b943338ea582aeb9b0a406d7fb75f3f62bc16a9d.tar.gz;
  }) {}}:


nixpkgs.mkShell {
  buildInputs = [ nixpkgs.vscode ];
}
