{ s }:
rec
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:lamport-spectacle' --allow-eval --warnings";
  allScripts = [ ghcidScript ];
}
