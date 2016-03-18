{ nixpkgs ? import <nixpkgs> {} }:
rec {
  packages = haskellngPackages.override { 
    inherit overrides;
  };
  overrides = self: super: {
      helium = enableParallell (setPostInstall ''
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
                  (echo "1 + 2" | $out/bin/texthint) || true
               '' (
               setSource ./.
                   (lib.addBuildTools
                     (self.callPackage ./helium.nix {})
                     [ self.cabal2nix
                       nixpkgs.makeWrapper
		       # self.ghc
                     ]
                   )));
      Top = self.callPackage ./top.nix {};
      lvmlib = self.callPackage ./lvmlib.nix {};
#      Top = let src = nixpkgs.fetchgit {
#                         url = "https://github.com/roberth/uu-top";
#                         rev = "c65dd102d";
# #                        sha256 = "c11a2da90a0cf743766c4f44ee9599b9f3dc750d8d119c94323bbe8454f05337";
#                       };
#             in setSource src super.Top;
  };
  inherit (packages) helium lvmlib Top;
  haskellngPackages = nixpkgs.haskellngPackages;
  lib = nixpkgs.haskell-ng.lib;
  enableParallell = pkg: nixpkgs.stdenv.lib.overrideDerivation pkg (oldAttrs: {
    configurePhase = ''
      configureFlags+=" --ghc-option=-j12"
      '' + oldAttrs.configurePhase;
  });
  setSource = (dir: pkg: 
          nixpkgs.stdenv.lib.overrideDerivation pkg (oldAttrs: { src = filterDir dir; }));
  setPostInstall = (pi : pkg:
     nixpkgs.stdenv.lib.overrideDerivation pkg (oldAttrs: { postInstall = pi; }));
  filterDir = builtins.filterSource (path: type: type != "unknown" 
		 && baseNameOf path != ".git"
                 && baseNameOf path != "dist"
                 && builtins.match "result.*" (baseNameOf path) == null
                 && builtins.match ".*.nix" (baseNameOf path) == null
                 );

}
