{ mkDerivation, base, containers, mtl, parsec, stdenv, fetchgit }:
mkDerivation {
  pname = "Top";
  version = "1.8";
  src = fetchgit {
                         url = "https://github.com/roberth/uu-top";
                         rev = "c65dd102d";
                         sha256 = "c11a2da90a0cf743766c4f44ee9599b9f3dc750d8d119c94323bbe8454f05337";
                       };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers mtl ];
  executableHaskellDepends = [ base containers mtl parsec ];
  homepage = "http://www.cs.uu.nl/wiki/bin/view/Helium/WebHome";
  description = "Constraint solving framework employed by the Helium Compiler";
  license = "GPL";
  }

