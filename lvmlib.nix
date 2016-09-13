{ mkDerivation, array, base, containers, directory, filepath
, parsec, stdenv, fetchgit, wl-pprint
}:
mkDerivation {
  pname = "lvmlib";
  version = "1.2";
  src = fetchgit {
                         url = "https://github.com/roberth/uu-lvm";
                         rev = "4d62a7c47ff932298ce6e54415f4bad411c7ed3f";
                         sha256 = "1rdxm7z2j2950gpswh89pz96jldl8mq89alfv8k8crmlmhc66qnm";
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
