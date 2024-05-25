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
      devShells.default = pkgs.mkShell {
        packages = [
          pkgs.cabal-install
          hsPkgs.haskell-language-server
          hsPkgs.fourmolu
          hsPkgs.cabal-fmt
        ];
      };
    });
}
