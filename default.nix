with (import ./nix/default.nix);
haskell.lib.dontCheck haskellPackages.unix-recursive
