{ pkgs ? import ./nix/nixpkgs.nix { config = import ./nix/config.nix; } }:

pkgs.haskellPackages.callCabal2nix "aoc" ./. {}
