{
  description = "Pegasus project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    cardano-node.url = "github:intersectmbo/cardano-node/8.9.0";
  };

  outputs =
    { flake-utils, ... } @ inputs:
    flake-utils.lib.eachSystem flake-utils.lib.defaultSystems (system:
    let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          # Contains libsodium-vrf, libblst and libsecp25k1 libraries
          inputs.iohk-nix.overlays.crypto
        ];
      };

      hsPkgs = pkgs.haskellPackages;

      ghcWithPackages = pkgs.haskell.packages.ghc96.ghcWithPackages (ps: with ps; [
        # Nix-provided libraries (no need to rebuild)
        # XXX: Annoying to keep updated with .cabal build-depends
        # library
        aeson
        bytestring
        directory
        file-embed
        filepath
        microlens
        microlens-aeson
        text
        time
        typed-process
        unix
        # executable
        pretty-simple
        # tests
        hspec
        HUnit
      ]);

      mkShell = { isCI ? false }:
        pkgs.mkShell {
          packages =
            let
              libs = [
                pkgs.pkg-config
                pkgs.libsodium-vrf
                pkgs.blst
                pkgs.secp256k1
                pkgs.zlib
              ];
              tools = [
                pkgs.cabal-install
                ghcWithPackages
              ];
              devTools = [
                hsPkgs.haskell-language-server
                hsPkgs.fourmolu
                hsPkgs.cabal-fmt
                hsPkgs.hoogle
              ];
            in
            libs ++ tools ++
            pkgs.lib.optionals (isCI == false) devTools ++
            # Cardano-node to build against
            pkgs.lib.optional (isCI == false) inputs.cardano-node.packages.${system}.cardano-node
          ;
        };

    in
    {
      legacyPackages = pkgs;

      devShells.default = mkShell { };
      # No dev tools and without cardano-node to run integration tests
      devShells.ci = mkShell { isCI = true; };
    });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
