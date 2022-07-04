{
  description = "WebGear Project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] (system:
      let
        mapcat = f: lst: builtins.foldl' (l: r: l // r) {} (map f lst);

        pkgs = nixpkgs.legacyPackages.${system};

        hsVersions = ["ghc923" "ghc902" "ghc8107"];
        hsDefaultVersion = "ghc923";

        haskellPackages = mapcat (hsVersion: {
          ${hsVersion} = pkgs.haskell.packages.${hsVersion}.override {
            # Add any package overrides here
            # overrides = final: prev: {
            #  package-name = final.callPackage ./nix/haskell-modules/package-name-version.nix {};
            # };
          };
        }) hsVersions;

        localPackages = hsVersion:
          let
            hsPkgs = haskellPackages.${hsVersion};
            webgear-core = hsPkgs.callCabal2nix "webgear-core" ./webgear-core {};
          in {
            "webgear-core-${hsVersion}" = webgear-core;
            "webgear-server-${hsVersion}" = hsPkgs.callCabal2nix "webgear-server" ./webgear-server {
              inherit webgear-core;
            };
            "webgear-openapi-${hsVersion}" = hsPkgs.callCabal2nix "webgear-openapi" ./webgear-openapi {
              inherit webgear-core;
            };
          };

        mkDevShell = hsVersion:
          let hsPkgs = haskellPackages.${hsVersion};
              shell = hsPkgs.shellFor {
                name = "webgear-dev-${hsVersion}";
                doBenchmark = true;

                packages = p: map (name: p.${name})
                  ["webgear-core" "webgear-server" "webgear-openapi"];

                buildInputs = [
                  hsPkgs.ghc
                  haskellPackages.${hsDefaultVersion}.fourmolu
                  pkgs.cabal-install
                  pkgs.hlint
                  pkgs.haskell-language-server
                  # (haskellPackages.${hsDefaultversion}.packdeps
                  pkgs.stack
                ];

                inputsFrom = builtins.attrValues self.packages.${system};
              };
          in { ${shell.name} = shell; };
      in {
        packages = mapcat localPackages hsVersions;

        devShells = mapcat mkDevShell hsVersions;
      });
}
