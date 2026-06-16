let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { config = {}; };
  project = import ./default.nix {};
in
  project.env.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.hpack
    ];
  })
