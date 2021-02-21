with (import ./default.nix);
mkShell {
  name = "unix-recursive-cabal-shell";
  buildInputs = [ stack php psrecord fourmolu ];
  inputsFrom = [ (haskell.lib.doBenchmark haskellPackages.unix-recursive).env ];
}
