{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./bluesky-tools.nix {}
