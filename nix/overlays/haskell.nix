{ gitignore }:

final: prev:

let
  hsLib = final.haskell.lib.compose;
  mapcat = f: lst: builtins.foldl' (l: r: l // r) {} (map f lst);

  ghcVersions = ["981" "964" "948" "928" "902"];
  defaultGHCVersion = "981";

  localHsPackages = {
    # Libraries
    "webgear-core" = ../../webgear-core;
    "webgear-server" = ../../webgear-server;
    "webgear-swagger" = ../../webgear-swagger;
    "webgear-swagger-ui" = ../../webgear-swagger-ui;
    "webgear-openapi" = ../../webgear-openapi;
    "webgear-benchmarks" = ../../webgear-benchmarks;

    # Examples
    "webgear-example-users" = ../../webgear-example-users;
    "webgear-example-realworld" = ../../webgear-example-realworld;
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
    packages = prev.haskell.packages // {
      ghc981 = prev.haskell.packages.ghc981.override {
        overrides = hfinal: hprev:
          (final.lib.mapAttrs (mkLocalDerivation hfinal) localHsPackages) // {
            # Need specific versions for benchmarking
            scotty = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/scotty.nix {});
            servant = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant.nix {});
            servant-server = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant-server.nix {});

            # doctests fail
            openapi3 = hsLib.dontCheck (hsLib.unmarkBroken hprev.openapi3);

            # For base-4.19 && bytestring-0.12
            postgresql-libpq = hprev.postgresql-libpq_0_10_0_0;

            # For th-abstraction-0.6
            generics-sop = hprev.generics-sop_0_5_1_4;

            # Unmaintained package with test fail
            hourglass = hsLib.dontCheck hprev.hourglass;

            # For base-4.19 && deepseq-1.5
            singleton-bool = hprev.singleton-bool_0_1_7;

            # For base-4.19 && hedgehog-1.4
            tasty-hedgehog = hsLib.doJailbreak hprev.tasty-hedgehog_1_4_0_2;

            # For text-2.1
            attoparsec-iso8601 = hprev.attoparsec-iso8601_1_1_0_1;

            # Test failures
            bsb-http-chunked = hsLib.dontCheck hprev.bsb-http-chunked;

            # For text-2.1
            http-api-data = hprev.http-api-data_0_6;

            # For base-4.19
            postgresql-simple = hprev.postgresql-simple_0_7_0_0;

            # For base-4.19
            swagger2 = hfinal.callPackage ../haskell-packages/swagger2.nix {};

            # For aeson-2.2. Cannot upgrade because that requires
            # optparse-applicative-0.18.1.0 which causes infinite
            # recursion
            criterion = hsLib.doJailbreak hprev.criterion;

            # For text-2.1. Plus test failures.
            esqueleto = hsLib.dontCheck (hfinal.callPackage ../haskell-packages/esqueleto.nix {});
          };
      };

      ghc964 = prev.haskell.packages.ghc964.override {
        overrides = hfinal: hprev:
          final.lib.mapAttrs (mkLocalDerivation hfinal) localHsPackages // {
            # Need specific versions for benchmarking
            scotty = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/scotty.nix {});
            servant = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant.nix {});
            servant-server = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant-server.nix {});

            # doctests fail
            openapi3 = hsLib.dontCheck (hsLib.unmarkBroken hprev.openapi3);

            # For th-abstraction-0.6
            generics-sop = hprev.generics-sop_0_5_1_4;
          };
      };

      ghc948 = prev.haskell.packages.ghc948.override {
        overrides = hfinal: hprev:
          final.lib.mapAttrs (mkLocalDerivation hfinal) localHsPackages // {
            # Need specific versions for benchmarking
            scotty = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/scotty.nix {});
            servant = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant.nix {});
            servant-server = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant-server.nix {});

            # doctests fail
            openapi3 = hsLib.dontCheck (hsLib.unmarkBroken hprev.openapi3);
          };
      };

      ghc928 = prev.haskell.packages.ghc928.override {
        overrides = hfinal: hprev:
          final.lib.mapAttrs (mkLocalDerivation hfinal) localHsPackages // {
            # Need specific versions for benchmarking
            scotty = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/scotty.nix {});
            servant = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant.nix {});
            servant-server = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant-server.nix {});

            # doctests fail
            openapi3 = hsLib.dontCheck (hsLib.unmarkBroken hprev.openapi3);
          };
      };

      ghc902 = prev.haskell.packages.ghc902.override {
        overrides = hfinal: hprev:
          final.lib.mapAttrs (mkLocalDerivation hfinal) localHsPackages // {
            # Need specific versions for benchmarking
            scotty = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/scotty.nix {});
            servant = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant.nix {});
            servant-server = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant-server.nix {});

            # doctests fail
            openapi3 = hsLib.dontCheck (hsLib.unmarkBroken hprev.openapi3);
          };
      };
    };
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
            hsPkgs.ghc
            haskell-language-server
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
