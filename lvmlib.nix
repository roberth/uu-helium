{ mkDerivation, array, base, containers, directory, filepath
, parsec, stdenv, fetchgit, wl-pprint
}:
mkDerivation {
  pname = "lvmlib";
  version = "1.2";
  src = fetchgit {
                         url = "https://github.com/roberth/uu-lvm";
                         rev = "a8060cd143a74b3a5bab0145cf7255fe32f434bc";
                         sha256 = "054kkyabif6dba1ljsm0d632v17by2sg4nkq6qgz6qi4z18lgf9z";
                       };
  postUnpack = "export sourceRoot=$sourceRoot/src/lib; pwd";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base containers directory filepath parsec wl-pprint
  ];
  executableHaskellDepends = [
    array base containers directory filepath parsec wl-pprint
  ];
  homepage = "http://www.cs.uu.nl/wiki/bin/view/Helium/WebHome";
  description = "The Lazy Virtual Machine (LVM)";
  license = stdenv.lib.licenses.bsd3;
}
