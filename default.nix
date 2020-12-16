{ pkgs ? import ./nix/nixpkgs.nix { config = import ./nix/config.nix; } }:

let
  aoc = pkgs.haskellPackages.callCabal2nix "aoc" ./. {};
in
  pkgs.haskell.lib.overrideCabal aoc (old: {
    buildTools = (old.buildTools or []) ++ [
      pkgs.haskellPackages.cabal-install
    ];
  })
