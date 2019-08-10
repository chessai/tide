{ nixpkgs  ? import ./nixpkgs.nix
, compiler ? "ghc865"
}:

with rec {

  overlay = self: super:
    with rec {
      inherit (super) lib;
      hlib = super.haskell.lib;
      cleanHaskell = import ./cleanHaskell.nix { inherit lib; };

      haskellOverlay = hself: hsuper: {
        # Package overlays
        tide = hself.callCabal2nix "tide" (cleanHaskell ../.) { };
      };
    };

    {
      haskell = super.haskell // {
        packages = super.haskell.packages // {
          "${compiler}" = super.haskell.packages."${compiler}".override {
            overrides = super.lib.composeExtensions (super.haskell.packageOverrides or (_: _: {})) haskellOverlay;
          };
        };
      };
    };

  pkgs = import nixpkgs {
    overlays = [ overlay ];
    config = {
      allowUnfree = true;
      allowBroken = false;
    };
  };

};

{
  tide = pkgs.haskell.packages.${compiler}.tide;
}
