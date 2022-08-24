{
  description = "WebGear Project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      # Use the same nixpkgs
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, gitignore }:
    {
      overlays.default = import ./nix/overlays/haskell.nix { inherit gitignore; };
    }
    // flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };

        mkVersionedPackages = hsVersion:
          pkgs.lib.attrsets.mapAttrs' (name: _: {
            name = "${name}-${hsVersion}";
            value = pkgs.haskell.packages.${hsVersion}.${name};
          }) pkgs.localHsPackages;
      in {
        packages = pkgs.lib.mapcat mkVersionedPackages pkgs.hsVersions;

        devShells = pkgs.lib.mapcat pkgs.mkDevShell pkgs.hsVersions;
      });
}
