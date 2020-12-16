{
  packageOverrides = pkgs: {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: {
        delude = self.callPackage (builtins.fetchGit {
          name = "delude-0.1.0.3";
          url  = "https://github.com/chuahou/delude";
          ref  = "refs/tags/v0.1.0.3";
        }) {};
      };
    };
  };
}
