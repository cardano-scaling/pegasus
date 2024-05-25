# <p align="center">Pegasus 🐴🪶</p>

<div align="center">
  <p>Fast, one-click, seeded devnet for Cardano</p>
</div>

Pegasus is a thin wrapper around a `cardano-node` to get Cardano developers _off the ground_ in a single command:

``` shell
pegasus
```

TODO: insert gif here

## Why pegasus

Pegasus, the flying horse from greek mythology should get you off the ground quickly .. literally.

When developing applications and tools for Cardano, the need for a fast and deterministic sandbox environment in which to experiment with transactions running on-chain validators or run fast integration tests for off- and on-chain code arises quickly.

The principles and goals of this project are:

- Fast startup & teardown (< 1 second)
- Single binary installation on all platforms (< 5 min setup)
- Usable from the terminal and any application framework (command line options + files as interfaces)

## Use pegasus instead of

- `cardano-node` directly, when you don't want to bother with configuring and seeding a local node manually. Pegasus can also be used to scaffold a full `cardano-node` configuration and seeded network which can later be used by any compatible `cardano-node`.

- `plutip`: if you prefer downloading a single binary over using `nix`.

- `yaci-devkit`, when you need faster startup or not want docker containers but plain sub-processes.

## Installation

Pre-built binaries are available for Linux x86_64 and MacOS arm64:

``` shell
curl TODO | unzip 
./pegasus --help
```

## Configuration

By default, `pegasus` starts a block-producing `cardano-node` in a temporary directory.

## Credits

Inspired by `anvil` of the Foundry development kit (TODO add link), this was created from glue code previously built for `hydra`, which now uses `pegasus` to setup devnets for end-to-end testing.

## Contributing

### Building

To build and run using [nix](https://nixos.org/):

``` shell
nix run
```

Or install `GHC` and `cabal` (e.g. using [GHCup](https://www.haskell.org/ghcup/)) and:

```sh
cabal run
```

### How it works

- Similar like other devnet tools, `pegasus` sets up a local cluster of `cardano-node`s
- A key difference is, that it comes with a `cardano-node` binary embedded
- Both `pegasus` and the included `cardano-node` are statically linked and have no or minimal dependencies

### TODO

#### Basics

- [x] Embed `cardano-node` at build time mechanism
- [ ] Embed config and start a devnet

#### Distribution 
- [ ] Redistributable executable (static libs)
- [ ] Pre-built binaries and "curl install"
- [ ] Reduce size of binaries
- [ ] Full static exeutable (?)
- [ ] Fill TODOs in readme

#### Configurability
- [ ] Configurable block time `--block-time 0.1`
- [ ] Use provided cardano-node `--use-cardano-node <path-to-exe>`
- [ ] Configurable era, e.g. `--babbage`
- [ ] Use and keep state directory when asked with `--directory <dir>`
