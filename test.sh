#!/usr/bin/env bash

# Integration test suite for pegasus
#
# This is defined external to the cabal project to ensure the promised way of
# wrapping and running cardano-node is in fact working.
#
#TODO: convert this to a bash test (BAT?)

nix build
[ ./result/pegasus --help | grep "pegasus" ] || "Expected help"
