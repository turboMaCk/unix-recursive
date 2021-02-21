with (import ./default.nix);
(haskell.lib.doBenchmark haskellPackages.unix-recursive).env
