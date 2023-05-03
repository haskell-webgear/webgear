{ gitignore }:

final: prev:

let
  mapcat = f: lst: builtins.foldl' (l: r: l // r) {} (map f lst);

  hsVersions = ["ghc961" "ghc944" "ghc926" "ghc902" "ghc8107"];
  hsDefaultVersion = "ghc961";

  localHsPackages = {
    "webgear-core" = ../../webgear-core;
    "webgear-server" = ../../webgear-server;
    "webgear-openapi" = ../../webgear-openapi;
    "webgear-benchmarks" = ../../webgear-benchmarks;
  };

  mkLocalDerivation = hspkgs: name: path:
    let
      pkg = hspkgs.callCabal2nix name (gitignore.lib.gitignoreSource path) {};
    in
      haskell.lib.overrideCabal pkg (old: {
        doHaddock = true;
        doCheck = true;
      });

  haskell = prev.haskell // {
    packages = prev.haskell.packages // mapcat (hsVersion: {
      ${hsVersion} = prev.haskell.packages.${hsVersion}.override {
        overrides = hfinal: hprev:
          final.lib.mapAttrs (mkLocalDerivation hfinal) localHsPackages
          // {
            openapi3 = hfinal.callPackage ../haskell-packages/openapi3-3.2.3.nix {};
          };
      };
    }) hsVersions;
  };

  mkDevShell = hsVersion:
    let hsPkgs = haskell.packages.${hsVersion};
        shell = hsPkgs.shellFor {
          name = "webgear-dev-${hsVersion}";
          doBenchmark = true;

          packages = pkgs: map (name: pkgs.${name}) (builtins.attrNames localHsPackages);

          buildInputs = [
            final.cabal-install
            final.cabal2nix
            haskell.packages.${hsDefaultVersion}.fourmolu
            hsPkgs.ghc
            final.hlint
            final.haskell-language-server
            final.stack
          ];

          src = null;
        };
    in { ${shell.name} = shell; };
in {
  inherit hsVersions hsDefaultVersion;

  lib = prev.lib // {
    inherit mapcat;
  };

  inherit localHsPackages haskell mkDevShell;
}
