# WebGear Examples - User
This is a basic CRUD app that operates on user resources.

# Building and running

```shell
nix run .#webgear-example-users-ghc${GHC_VERSION}
```

Or, if you'd like to get a development environment:

```shell
nix develop
cabal run webgear-example-users
```

Run postman tests (from another shell):

```shell
nix develop
newman run postman-collection.json
```
