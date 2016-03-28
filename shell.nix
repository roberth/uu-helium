{ nixpkgs ? import ./nixpkgs {} }:
let p = import ./packages.nix { inherit nixpkgs; };
in p.helium.env
