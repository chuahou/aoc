{ pkgs ? import ./nix/nixpkgs.nix { config = import ./nix/config.nix; } }:

let
  src = builtins.filterSource (path: type:
    (type != "directory" || baseNameOf path != ".dist-newstyle") &&
    (type != "symlink"   || baseNameOf path != "result")         &&
    (type != "directory" || baseNameOf path != ".github")        &&
    (type != "directory" || baseNameOf path != ".git"))          ./.;

in
  builtins.deepSeq
    (builtins.readFile ./package.yaml)
    (pkgs.haskellPackages.callCabal2nix "aoc" src {})
