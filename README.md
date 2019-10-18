
## Development

# Run ghcid
nix-shell -p haskellPackages.ghcid --run "ghcid -c 'cabal new-repl lib:tickGameLib'"

# Run tests
cabal new-test --disable-optimization

# Run the application
cabal new-run exe:tickGame --disable-optimization --enable-executable-dynamic

# Count lines of code
nix-shell -p cloc --run "cloc src/ lib/ tests/"
