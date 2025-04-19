{
  description = "WebGear Project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/haskell-updates";
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

        mkVersionedPackages = ghcVersion:
          pkgs.lib.attrsets.mapAttrs' (name: _: {
            name = "${name}-ghc${ghcVersion}";
            value = pkgs.haskell.packages."ghc${ghcVersion}".${name};
          }) pkgs.localHsPackages;

        versionedDevShells = pkgs.lib.mapcat pkgs.mkDevShell pkgs.ghcVersions;
      in {
        packages = pkgs.lib.mapcat mkVersionedPackages pkgs.ghcVersions;

        devShells = pkgs.hackageUploadShell // versionedDevShells // {
          default = versionedDevShells."webgear-dev-ghc${pkgs.defaultGHCVersion}";
        };
      });
}
