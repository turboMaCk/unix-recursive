let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        pkgs.haskellPackages.override {
            overrides = self: super: {
              unix-recursive = pkgs.haskell.lib.dontCheck (self.callCabal2nix "unix-recursive" ../. {});
            };
        };
    };
  };
in
import ./pkgs.nix { inherit config; }
