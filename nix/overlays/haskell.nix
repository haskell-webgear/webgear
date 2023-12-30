{ gitignore }:

final: prev:

let
  hsLib = final.haskell.lib.compose;
  mapcat = f: lst: builtins.foldl' (l: r: l // r) {} (map f lst);

  ghcVersions = ["981" "963" "948" "928" "902" "8107"];
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
          final.lib.mapAttrs (mkLocalDerivation hfinal) localHsPackages // {
            # Need specific versions for benchmarking
            scotty = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/scotty.nix {});
            servant = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant.nix {});
            servant-server = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant-server.nix {});

            # For ghc-9.8
            system-cxx-std-lib = null;
            aeson = hprev.aeson_2_2_1_0;
            bifunctors = hprev.bifunctors_5_6_1;
            free = hprev.free_5_2;
            generics-sop = hprev.generics-sop_0_5_1_4;
            hspec-core = hprev.hspec-core_2_11_7;
            hspec-meta = hprev.hspec-meta_2_11_7;
            some = hprev.some_1_0_6;
            tagged = hprev.tagged_0_8_8;
            th-abstraction = hprev.th-abstraction_0_6_0_0;

            # No longer broken
            bytestring-conversion = hsLib.unmarkBroken hprev.bytestring-conversion;

            # doctests fail
            openapi3 = hsLib.dontCheck (hsLib.unmarkBroken hprev.openapi3);
          };
      };

      ghc963 = prev.haskell.packages.ghc963.override {
        overrides = hfinal: hprev:
          final.lib.mapAttrs (mkLocalDerivation hfinal) localHsPackages // {
            # Need specific versions for benchmarking
            scotty = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/scotty.nix {});
            servant = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant.nix {});
            servant-server = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant-server.nix {});

            # No longer broken
            bytestring-conversion = hsLib.unmarkBroken hprev.bytestring-conversion;

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

            # No longer broken
            bytestring-conversion = hsLib.unmarkBroken hprev.bytestring-conversion;

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

            # No longer broken
            bytestring-conversion = hsLib.unmarkBroken hprev.bytestring-conversion;

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

            # No longer broken
            bytestring-conversion = hsLib.unmarkBroken hprev.bytestring-conversion;

            # doctests fail
            openapi3 = hsLib.dontCheck (hsLib.unmarkBroken hprev.openapi3);
          };
      };

      ghc8107 = prev.haskell.packages.ghc8107.override {
        overrides = hfinal: hprev:
          final.lib.mapAttrs (mkLocalDerivation hfinal) localHsPackages // {
            # Need specific versions for benchmarking
            scotty = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/scotty.nix {});
            servant = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant.nix {});
            servant-server = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant-server.nix {});

            # No longer broken
            bytestring-conversion = hsLib.unmarkBroken hprev.bytestring-conversion;

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
            final.hlint
            final.stack
            final.newman
          ] ++ final.lib.optionals (ghcVersion == defaultGHCVersion) [
            haskell.packages."ghc${defaultGHCVersion}".fourmolu
          ] ++ final.lib.optionals (ghcVersion != "8107") [
            # HLS no longer supports GHC-8.10
            haskell-language-server
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
