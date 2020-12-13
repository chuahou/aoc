{ pkgs ? import ./nix/nixpkgs.nix {} }:

let pkg = pkgs.haskellPackages.callCabal2nix "aoc" ./. {};
in pkgs.haskell.lib.overrideCabal pkg (old: {
  buildTools = (old.buildTools or []) ++ [
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.haskell-language-server
  ];
})
