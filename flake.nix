{
  description = "WebGear Project";

  inputs = {
    # nixos-22.05 on 2022-06-25
    nixpkgs.url = "github:NixOS/nixpkgs/ccf8bdf72624521358be6bb7d9b524c4cbcf7aff";
    # master on 2022-06-25
    flake-utils.url = "github:numtide/flake-utils/bee6a7250dd1b01844a2de7e02e4df7d8a0a206c";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc922.override {
          overrides = final: prev: {
            bytestring-conversion = final.callPackage ./nix/haskell-modules/bytestring-conversion-0.3.2.nix {};
          };
        };
      in {
        packages = rec {
          webgear-core = haskellPackages.callCabal2nix "webgear-core" ./webgear-core {};

          webgear-server = haskellPackages.callCabal2nix "webgear-server" ./webgear-server {
            inherit webgear-core;
          };

          webgear-openapi = haskellPackages.callCabal2nix "webgear-openapi" ./webgear-openapi {
            inherit webgear-core;
          };
        };

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            cabal2nix
            cabal-install
            ghcid
            haskellPackages.haskell-language-server
            # haskellPackages.packdeps
            stack
          ];

          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
