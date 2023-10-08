{ gitignore }:

final: prev:

let
  mapcat = f: lst: builtins.foldl' (l: r: l // r) {} (map f lst);

  hsVersions = ["ghc962" "ghc946" "ghc928" "ghc902" "ghc8107"];
  hsDefaultVersion = "ghc962";

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
          //
          (let
            hsLib = final.haskell.lib.compose;
          in {
            # jailbreak for text > 2.0
            bytestring-conversion = hsLib.doJailbreak (hsLib.unmarkBroken hprev.bytestring-conversion);

            # For ghc-9.6 support
            generics-sop = hsLib.doJailbreak hprev.generics-sop;
            optics-th = hsLib.doJailbreak hprev.optics-th;

            # Tests fail on GHC >9.2
            openapi3 = hsLib.dontCheck (hsLib.unmarkBroken hprev.openapi3);

            # Need specific versions for benchmarking
            servant = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant.nix {});
            servant-server = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant-server.nix {});
            scotty = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/scotty.nix {});
          });
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
