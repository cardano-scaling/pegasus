{
  description = "Pegasus project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    inputs@{ self, flake-utils, nixpkgs, ... }:
    flake-utils.lib.eachSystem flake-utils.lib.defaultSystems (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };

      hsPkgs = pkgs.haskellPackages.extend (pkgs.haskell.lib.compose.packageSourceOverrides {
        pegasus = ./.;
      });

      pegasus' = pkgs.haskell.lib.dontCheck hsPkgs.pegasus;
    in
    {
      packages.default = pegasus';

      devShells.default = hsPkgs.shellFor {
        packages = p: [ p.pegasus ];
        withHoogle = true;
        buildInputs = [
          pkgs.cabal-install
          hsPkgs.haskell-language-server
          hsPkgs.fourmolu
          hsPkgs.cabal-fmt
        ];
      };
    });
}
