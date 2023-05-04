{ gitignore }:

final: prev:

let
  mapcat = f: lst: builtins.foldl' (l: r: l // r) {} (map f lst);

  hsVersions = ["ghc944" "ghc926" "ghc902" "ghc8107"];
  hsDefaultVersion = "ghc944";

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
            bytestring-conversion = hfinal.callPackage ../haskell-packages/bytestring-conversion-0.3.2.nix {}; # jailbreak for text > 2.0
            generics-sop = hfinal.callPackage ../haskell-packages/generics-sop-0.5.1.3.nix {}; # For ghc-9.6 support
            openapi3 = hfinal.callPackage ../haskell-packages/openapi3-3.2.3.nix {}; # For doCheck = false. Doctests fail on GHC >9.2
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
            hsPkgs.ghc
            final.hlint
            final.haskell-language-server
            final.stack
          ] ++ final.lib.optionals (hsVersion == hsDefaultVersion) [
            haskell.packages.${hsDefaultVersion}.fourmolu
          ];

          src = null;
        };
    in { ${shell.name} = shell; };
in {
  inherit hsVersions;

  lib = prev.lib // {
    inherit mapcat;
  };

  inherit localHsPackages haskell mkDevShell;
}
