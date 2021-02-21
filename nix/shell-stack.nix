{ pkgs ? import ./pkgs.nix }:
with pkgs;
mkShell {
  name = "unix-recursive-stack-shell";
  buildInputs = [ stack php psrecord fourmolu ];
}
