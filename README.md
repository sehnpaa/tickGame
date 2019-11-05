
# Development

## Run ghcid
nix-shell -p haskellPackages.ghcid --run "ghcid -c 'cabal new-repl lib:tickGameLib'"

## Run tests
cabal new-test --disable-optimization

## Run the application
cabal new-run exe:tickGame --disable-optimization --enable-executable-dynamic

## Count lines of code
nix-shell -p cloc --run "cloc src/ lib/ tests/"

## Fix 'Operation not permitted' on Debian
`sysctl kernel.unprivileged_userns_clone=1`
https://github.com/NixOS/nix/issues/2633
