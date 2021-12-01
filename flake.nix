# SPDX-License-Identifier: MIT
# Copyright (c) 2021 Chua Hou

{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-20.09";
    delude = { url = "github:chuahou/delude/v0.1.0.4"; flake= false; };
  };

  outputs = inputs@{ nixpkgs, ... }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ (self: super: {
        haskellPackages = super.haskellPackages.override {
          overrides = self: super: {
            delude = super.callPackage inputs.delude {};
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
        ]);
      })).env;
  };
}
