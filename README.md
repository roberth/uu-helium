
[![Build Status](https://travis-ci.org/roberth/uu-helium.svg?branch=master)](https://travis-ci.org/roberth/uu-helium)

Helium is a compiler for a subset of Haskell that aims at delivering high
quality type error messages particularly for beginner programmers. It also
includes facilities for specializing type error diagnosis for embedded domain
specific languages. The compiler supports Haskell 98, but without the ability
to define new type classes and instances.

# Installing

## Recommended version

An official version of the helium compiler is available on hackage. It can be installed by running:

```bash
cabal install helium
cabal install lvmrun
```

## This development version

On OS X and Linux, clone this repository locally and run:

```bash
curl https://nixos.org/nix/install | sh
nix-env -f packages.nix -iA helium
```

The first line will install Nix, a “purely functional package manager”.
The second line will build helium and make it available in your user environment.

# Developing

To modify the helium compiler, clone this repository locally and run:

```bash
nix-shell
```

This will start a shell with access to all dependencies. In this shell, run

```bash
./compile
ln -s ../helium/helium dist/build/Helium/
(cd test && ./testAll)
```

# Resources

 * [Helium homepage](http://foswiki.cs.uu.nl/foswiki/Helium)
 * [Hackage](https://hackage.haskell.org/package/helium)
