{ config ? {} }:
with (import <nixpkgs> {});
import (fetchTarball {
  # 2021/02/21
  url = "https://github.com/NixOS/nixpkgs/archive/9816b99e71c3504b0b4c1f8b2e004148460029d4.tar.gz";
  sha256 = "1dpz36i3vx0c1wmacrki0wsf30if8xq3bnj71g89rsbxyi87lhcm";
}) {
  inherit config;
  overlays = [
    (self: super: {
      fourmolu = with self.haskell.lib; justStaticExecutables
        (overrideCabal self.haskellPackages.fourmolu (old: {
          version = "0.3.0.0";
          src = fetchTarball https://github.com/parsonsmatt/fourmolu/archive/45a8478b8e6ba48b4ce228d4aaee3cb9f5aa08f6.tar.gz;
        }));
    })
  ];
}
