# SPDX-License-Identifier: MIT
# Copyright (c) 2021 Chua Hou

{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.11";
    delude = {
      url = "github:chuahou/delude/v0.1.0.5";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ nixpkgs, ... }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ (self: super: {
        haskellPackages = super.haskellPackages.override {
          overrides = self: super: {
            delude = inputs.delude.defaultPackage.${system};
          };
        };
      }) ];
    };

  in rec {
    defaultPackage.${system} = pkgs.haskell.lib.dontHaddock
      (pkgs.haskellPackages.callCabal2nix "aoc" ./. {});

    devShell.${system} =
      (pkgs.haskell.lib.overrideCabal defaultPackage.${system} (old: {
        buildTools = (old.buildTools or []) ++ (with pkgs.haskellPackages; [
          cabal-install
          haskell-language-server
        ]);
      })).env;
  };
}
