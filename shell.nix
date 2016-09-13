{ nixpkgs ? import ./nixpkgs {} }:
let p = import ./packages.nix { inherit nixpkgs; };
    override = obj: f: derivation (obj.drvAttrs // f (obj.drvAttrs));
in  (override p.helium.env (super : {
          nativeBuildInputs = super.nativeBuildInputs ++ [p.haskellPackages.intero];
                    }))
