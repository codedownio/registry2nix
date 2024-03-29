{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        default = pkgs.haskell.packages.ghc928.callPackage ./default.nix {};
      in
        rec {
          packages = rec {
            inherit default;
            inherit (pkgs) cabal2nix;
          };

          defaultPackage = packages.default;

          nixpkgsPath = pkgs.path;
        }
    );
}
