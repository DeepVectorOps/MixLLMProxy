{ sources ? import ./nix/sources.nix
} :
let
  pkgs = import sources.nixpkgs { config = { allowBroken = true; }; };
  src = pkgs.lib.cleanSourceWith {
    filter = name: type: !(pkgs.lib.hasSuffix ".cabal" name);
    src = ./.;
  };
in
  pkgs.haskellPackages.callCabal2nix "HaskellNixCabalStarter" src {}
