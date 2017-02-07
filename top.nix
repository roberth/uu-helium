{ mkDerivation, base, containers, mtl, parsec, stdenv, fetchgit }:
mkDerivation {
  pname = "Top";
  version = "1.8";
  src = fetchgit {
                         url = "https://github.com/roberth/uu-top";
                         rev = "7661cf32c5a2ccee92fc1648fc17351c782f29f3";
                         sha256 = "1v74l3bfhnxk9slwjj8jhpy2dznh6bx8p0xa1li3sr771xs5wamm";
                       };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers mtl ];
  executableHaskellDepends = [ base containers mtl parsec ];
  homepage = "http://www.cs.uu.nl/wiki/bin/view/Helium/WebHome";
  description = "Constraint solving framework employed by the Helium Compiler";
  license = "GPL";
  }

