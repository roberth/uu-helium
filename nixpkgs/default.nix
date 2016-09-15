let hostNixPkgs = import <nixpkgs> {};
in import (hostNixPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    #branch: nixos-16.09-small
    rev = "feef017564c292e088de69e97db4071bf08c2853";
    sha256 = "1jaz4pwva4xg6r8x24g476apg6wr7gl45qv4dlpk109r0hxvm543";
})
