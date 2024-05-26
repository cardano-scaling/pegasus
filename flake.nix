{
  description = "Pegasus project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    cardano-node.url = "github:intersectmbo/cardano-node/8.9.0";
    horizon-cardano.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-cardano.git";
  };

  outputs =
    { flake-utils, ... } @ inputs:
    flake-utils.lib.eachSystem flake-utils.lib.defaultSystems (system:
    let
      pkgs = import inputs.nixpkgs {
        inherit system;
      };

      hsPkgs = inputs.horizon-cardano.legacyPackages.${system}.extend (self: super: {
        microlens-aeson = self.callHackage "microlens-aeson" "2.5.1" { };
        pegasus =
          pkgs.lib.trivial.pipe (self.callCabal2nix "pegasus" ./. { }) [
            # Cardano-node to build against
            (pkgs.haskell.lib.compose.addBuildTool inputs.cardano-node.packages.${system}.cardano-node)
            # Don't run (integration) tests
            pkgs.haskell.lib.compose.dontCheck
          ];
      });
    in
    {
      legacyPackages = pkgs;

      packages.default = hsPkgs.pegasus;

      devShells.default = hsPkgs.shellFor {
        packages = p: [ p.pegasus ];
        withHoogle = true;
        buildInputs = [
          pkgs.cabal-install
          pkgs.haskell.packages.ghc96.haskell-language-server
          pkgs.haskell.packages.ghc96.fourmolu
          pkgs.haskell.packages.ghc96.cabal-fmt
        ];
      };

      # NOTE: CI dev shell deliberatly does not contain cardano-node
      devShells.ci = (hsPkgs.extend (self: super: {
        pegasus = pkgs.haskell.lib.overrideCabal super.pegasus {
          buildTools = [ ];
        };
      })).shellFor {
        packages = p: [ p.pegasus ];
        buildInputs = [
          pkgs.cabal-install
        ];
      };
    });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://horizon.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "horizon.cachix.org-1:MeEEDRhRZTgv/FFGCv3479/dmJDfJ82G6kfUDxMSAw0="
    ];
  };
}
