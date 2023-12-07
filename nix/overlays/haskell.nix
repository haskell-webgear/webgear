{ gitignore }:

final: prev:

let
  mapcat = f: lst: builtins.foldl' (l: r: l // r) {} (map f lst);

  ghcVersions = ["962" "946" "928" "902" "8107"];
  defaultGHCVersion = "962";

  localHsPackages = {
    # Libraries
    "webgear-core" = ../../webgear-core;
    "webgear-server" = ../../webgear-server;
    "webgear-swagger" = ../../webgear-swagger;
    "webgear-openapi" = ../../webgear-openapi;
    "webgear-benchmarks" = ../../webgear-benchmarks;

    # Examples
    "webgear-example-users" = ../../webgear-example-users;
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
    packages = prev.haskell.packages // mapcat (ghcVersion: {
      "ghc${ghcVersion}" = prev.haskell.packages."ghc${ghcVersion}".override {
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
    }) ghcVersions;
  };

  mkDevShell = ghcVersion:
    let hsPkgs = haskell.packages."ghc${ghcVersion}";

        haskell-language-server = prev.haskell-language-server.override {
          supportedGhcVersions = [ ghcVersion ];
        };

        shell = hsPkgs.shellFor {
          name = "webgear-dev-ghc${ghcVersion}";
          doBenchmark = true;

          packages = pkgs: map (name: pkgs.${name}) (builtins.attrNames localHsPackages);

          buildInputs = [
            final.cabal-install
            final.cabal2nix
            haskell-language-server
            hsPkgs.ghc
            final.hlint
            final.stack
            final.newman
          ] ++ final.lib.optionals (ghcVersion == defaultGHCVersion) [
            haskell.packages."ghc${defaultGHCVersion}".fourmolu
          ];

          src = null;
        };
    in { ${shell.name} = shell; };
in {
  inherit ghcVersions defaultGHCVersion;

  lib = prev.lib // {
    inherit mapcat;
  };

  inherit localHsPackages haskell mkDevShell;
}
