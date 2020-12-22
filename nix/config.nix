{
  packageOverrides = pkgs: {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: {
        delude = self.callPackage (builtins.fetchGit {
          name = "delude-0.1.0.3";
          url  = "https://github.com/chuahou/delude";
          ref  = "refs/tags/v0.1.0.3";
        }) {};

        # use latest HLS version
        haskell-language-server =
          let
            pkgs = import (builtins.fetchGit {
              name = "nixpkgs-haskell-updates-hls-0.7.1";
              url  = "https://github.com/nixos/nixpkgs";
              ref  = "refs/heads/haskell-updates";
              rev  = "a4530367944e55dbbd1d127123f3519b7483bc54";
            }) {};
          in
            pkgs.haskell-language-server;
      };
    };
  };
}
