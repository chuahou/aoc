{ pkgs ? import ./nix/nixpkgs.nix {}, shellEnv ? false }:

pkgs.haskellPackages.developPackage {
  root = ./.;
  source-overrides = {
    delude = builtins.fetchGit {
      name = "delude-0.1.0.2";
      url  = "https://github.com/chuahou/delude";
      ref  = "refs/tags/v0.1.0.2";
    };
  };
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.haskell-language-server
      pkgs.haskellPackages.stylish-haskell
    ];
  returnShellEnv = shellEnv;
}
