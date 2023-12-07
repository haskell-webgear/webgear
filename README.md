# WebGear - Build HTTP APIs

[![Hackage](https://img.shields.io/hackage/v/webgear-core)](https://hackage.haskell.org/package/webgear-core)
[![Hackage](https://img.shields.io/hackage/v/webgear-server)](https://hackage.haskell.org/package/webgear-server)
[![Hackage](https://img.shields.io/hackage/v/webgear-openapi)](https://hackage.haskell.org/package/webgear-openapi)

[![Build Status](https://img.shields.io/github/actions/workflow/status/haskell-webgear/webgear/ci.yaml?branch=main)](https://github.com/haskell-webgear/webgear/actions/workflows/ci.yaml)

WebGear is a Haskell library for building composable, type-safe HTTP APIs.

This is the main repository of WebGear project. It contains multiple packages:

- `webgear-core`: The core library.
- `webgear-server`: Serve WebGear applications using `wai` and `warp`.
- `webgear-swagger`: Generate Swagger 2.0 specifications from WebGear specifications.
- `webgear-openapi`: Generate OpenAPI specifications from WebGear specifications.

Examples of WebGear applications can be found at:

- https://github.com/haskell-webgear/webgear/webgear-example-users
- https://github.com/haskell-webgear/webgear/webgear-example-realworld

Documentation is available at https://haskell-webgear.github.io

## Development

Use Nix to start a reproducible development environment:

```shell
nix develop
```

This starts a shell with the default GHC. You can also use a specific GHC version with:

```shell
nix develop .#webgear-dev-ghc<GHC-VERSION>
```

You can use the standard cabal commands in the development shell.

You can build packages using Nix:

```shell
nix build .#webgear-core-ghc<GHC-VERSION> .#webgear-server-ghc<GHC-VERSION> .#webgear-swagger-ghc<GHC-VERSION> .#webgear-openapi-ghc<GHC-VERSION>
```
