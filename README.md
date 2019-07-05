nix-shell -p haskellPackages.ghcid --run "ghcid -c 'cabal new-repl lib:tickGameLib'"

cabal new-test --disable-optimization

cabal new-run exe:tickGame --disable-optimization --enable-executable-dynamic
