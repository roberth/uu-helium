{ nixpkgs ? import ./nixpkgs {} }:
rec {
  packages = haskellPackages.override {
    inherit overrides;
  };
  overrides = self: super: {
      helium = enableParallell (
               setPreBuild ''
                  (cd src && make)
                  '' (
               setPostInstall ''
                  # Link to low-level tools
                  ln -s ${self.lvmrun}/bin/lvmrun $out/bin/
                  ln -s ${self.lvmlib}/bin/coreasm $out/bin/
                  wrapProgram $out/bin/texthint \
                     --suffix PATH : $out/bin \
                     --suffix PATH : ${self.lvmrun}/bin \
                     --suffix PATH : ${self.lvmlib}/bin
                  wrapProgram $out/bin/runhelium \
                     --suffix PATH : $out/bin \
                     --suffix PATH : ${self.lvmrun}/bin \
                     --suffix PATH : ${self.lvmlib}/bin
                  wrapProgram $out/bin/helium \
                     --suffix PATH : $out/bin \
                     --suffix PATH : ${self.lvmrun}/bin \
                     --suffix PATH : ${self.lvmlib}/bin
                  echo "Precompiling core files using texthint..."
                  ((echo "1 + 2"; echo ":q") | $out/bin/texthint) || true
		  # Build .hs files
		  # Rebuilding them is not possible after installation.
		  # FIXME: the simple/ files don't seem to work
		  find $out/share -name "*.hs" -not -path "*simple*" | while read F
		  do  echo "Compiling $F with helium"
                      $out/bin/helium "$F"
		  done
                  '' (
               setSource ./.
                   (lib.addBuildTools
                     (self.callPackage ./helium.nix {})
                     [ nixpkgs.makeWrapper
		                   nixpkgs.cabal2nix
                       nixpkgs.cabal-install
                       haskellPackages.lvmrun
                       haskellPackages.uuagc
                     ]
                   ))));
      Top = self.callPackage ./top.nix {};
      # lvmlib = setSource (../uu-lvm) (self.callPackage ../uu-lvm/src/lib/lvmlib.nix {});
      lvmlib = self.callPackage ./lvmlib.nix {};
  };
  inherit (packages) helium lvmlib Top;
  inherit nixpkgs;

  # lvmlib is currently broken on GHC 8
  # haskellPackages = nixpkgs.haskellPackages;
  haskellPackages = nixpkgs.haskell.packages.ghc7103;

  lib = nixpkgs.haskell.lib;
  enableParallell = pkg: nixpkgs.stdenv.lib.overrideDerivation pkg (oldAttrs: {
    configurePhase = ''
      configureFlags+=" --ghc-option=-j12"
      '' + oldAttrs.configurePhase;
  });
  setSource = (dir: pkg:
          nixpkgs.stdenv.lib.overrideDerivation pkg (oldAttrs: { src = filterDir dir; }));
  setPostInstall = (pi : pkg:
     nixpkgs.stdenv.lib.overrideDerivation pkg (oldAttrs: { postInstall = pi; }));
  setPreBuild = (pb : pkg:
     nixpkgs.stdenv.lib.overrideDerivation pkg (oldAttrs: { preBuild = pb; }));
  filterDir = builtins.filterSource (path: type: type != "unknown"
		 && baseNameOf path != ".git"
                 && baseNameOf path != "dist"
                 && builtins.match "result.*" (baseNameOf path) == null
                 && builtins.match ".*.nix" (baseNameOf path) == null
                 );

}
