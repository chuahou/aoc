{ pkgs ? import ./nix/nixpkgs.nix { config = import ./nix/config.nix; } }:

builtins.deepSeq
  (builtins.readFile ./package.yaml)
  (pkgs.haskellPackages.callCabal2nix "aoc" ./. {})
