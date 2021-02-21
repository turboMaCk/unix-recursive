{ cabal ? false }:
if cabal then
    import ./nix/shell-cabal.nix
else
    import ./nix/shell-stack.nix {}
