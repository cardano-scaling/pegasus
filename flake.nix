{
  description = "Pegasus project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , iohk-nix
    , ...
    } @ inputs:
    flake-utils.lib.eachSystem flake-utils.lib.defaultSystems (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          # Contains libsodium-vrf, libblst and libsecp25k1 libraries
          inputs.iohk-nix.overlays.crypto
        ];
      };

      hsPkgs = pkgs.haskellPackages;

      ghcWithPackages = pkgs.haskell.packages.ghc98.ghcWithPackages (ps: with ps; [
        # Nix-provided libraries (no need to rebuild)
        # XXX: Annoying to keep updated with .cabal build-depends
        # library
        aeson
        bytestring
        directory
        filepath
        microlens
        microlens-aeson
        text
        time
        typed-process
        # executable
        pretty-simple
        # tests
        hspec
        HUnit
      ]);
    in
    {
      legacyPackages = pkgs;

      devShells.default = pkgs.mkShell {
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
              hsPkgs.haskell-language-server
              hsPkgs.fourmolu
              hsPkgs.cabal-fmt
            ];
          in
          libs ++ tools;
      };
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
