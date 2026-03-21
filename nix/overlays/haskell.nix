{ gitignore }:

final: prev:

let
  hsLib = final.haskell.lib.compose;
  mapcat = f: lst: builtins.foldl' (l: r: l // r) {} (map f lst);

  ghcVersions = ["9123" "9103" "984" "967" "948"];
  defaultGHCVersion = "9123";

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
      ghc9123 = prev.haskell.packages.ghc9123.override {
        overrides = hfinal: hprev:
          (final.lib.mapAttrs (mkLocalDerivation hfinal) localHsPackages) // {
            # Need specific versions for benchmarking
            scotty = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/scotty.nix {});
            servant = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant.nix {});
            servant-server = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant-server.nix {});

            # Latest packages
            jose = hprev.jose_0_12;

            # Tests fail
            cabal-add = hsLib.dontCheck hprev.cabal-add;
            fourmolu = hsLib.dontCheck hprev.fourmolu;
          };
      };

      ghc9103 = prev.haskell.packages.ghc9103.override {
        overrides = hfinal: hprev:
          (final.lib.mapAttrs (mkLocalDerivation hfinal) localHsPackages) // {
            # Need specific versions for benchmarking
            scotty = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/scotty.nix {});
            servant = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant.nix {});
            servant-server = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant-server.nix {});

            # For base-4.20
            openapi3 = hsLib.doJailbreak hprev.openapi3;
          };
      };

      ghc984 = prev.haskell.packages.ghc984.override {
        overrides = hfinal: hprev:
          (final.lib.mapAttrs (mkLocalDerivation hfinal) localHsPackages) // {
            # Need specific versions for benchmarking
            scotty = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/scotty.nix {});
            servant = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant.nix {});
            servant-server = hsLib.dontHaddock (hfinal.callPackage ../haskell-packages/servant-server.nix {});

            # doctests fail
            openapi3 = hsLib.dontCheck (hsLib.unmarkBroken hprev.openapi3);
          };
      };

      ghc967 = prev.haskell.packages.ghc967.override {
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
