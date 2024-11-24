{ gitignore }:

final: prev:

let
  hsLib = final.haskell.lib.compose;
  mapcat = f: lst: builtins.foldl' (l: r: l // r) {} (map f lst);

  ghcVersions = ["9101" "982" "966" "948" "928" "902"];
  defaultGHCVersion = "9101";

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
      ghc9101 = prev.haskell.packages.ghc9101.override {
        overrides = hfinal: hprev:
          (final.lib.mapAttrs (mkLocalDerivation hfinal) localHsPackages) // {
            # Need specific versions for benchmarking
            scotty = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/scotty.nix {});
            servant = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant.nix {});
            servant-server = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant-server.nix {});

            # Latest package versions
            generics-sop = hprev.generics-sop_0_5_1_4;
            insert-ordered-containers = hsLib.doJailbreak hprev.insert-ordered-containers;
            microstache = hprev.microstache_1_0_3;

            # For base-4.20
            openapi3 = hsLib.doJailbreak hprev.openapi3;
          };
      };

      ghc982 = prev.haskell.packages.ghc982.override {
        overrides = hfinal: hprev:
          (final.lib.mapAttrs (mkLocalDerivation hfinal) localHsPackages) // {
            # Need specific versions for benchmarking
            scotty = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/scotty.nix {});
            servant = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant.nix {});
            servant-server = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant-server.nix {});

            # doctests fail
            openapi3 = hsLib.dontCheck (hsLib.unmarkBroken hprev.openapi3);

            # Latest package versions
            network = hprev.network_3_2_4_0;
            lens = hprev.lens_5_3_2;

            # For network-3.2
            http2 = hsLib.doJailbreak hprev.http2;
            insert-ordered-containers = hsLib.doJailbreak hprev.insert-ordered-containers;
          };
      };

      ghc966 = prev.haskell.packages.ghc966.override {
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

  # Shell used to upload packages to hackage; contains a minimal set
  # of dependencies
  hackageUploadShell =
    let hsPkgs = haskell.packages."ghc${defaultGHCVersion}";

        shell = hsPkgs.shellFor {
          name = "webgear-hackage-upload-shell";
          doBenchmark = false;

          packages = pkgs: map (name: pkgs.${name}) (builtins.attrNames localHsPackages);

          buildInputs = [
            final.cabal-install
            final.cabal2nix
            final.curl
            final.findutils
            hsPkgs.ghc
          ];

          src = null;
        };
    in { ${shell.name} = shell; };
in {
  inherit ghcVersions defaultGHCVersion;

  lib = prev.lib // {
    inherit mapcat;
  };

  inherit localHsPackages haskell mkDevShell hackageUploadShell;
}
