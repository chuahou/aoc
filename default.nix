{ pkgs ? import ./nix/nixpkgs.nix { config = import ./nix/config.nix; } }:

let
  src = pkgs.nix-gitignore.gitignoreSource ''
    /.github/
    /.git/
  '' ./.;

in
  builtins.deepSeq
    (builtins.readFile ./package.yaml)
    (pkgs.haskell.lib.dontHaddock
      (pkgs.haskellPackages.callCabal2nix "aoc" src {}))
