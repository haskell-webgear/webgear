# WebGear Benchmarking
Some benchmarks comparing webgear against other Haskell web frameworks.

To run this benchmark:

```shell
nix develop .#webgear-dev-ghc<GHC-VERSION>
cabal run webgear-benchmarks:benchmarks -- --output criterion-report.html
```

### Sample Results

[Criterion Report](./criterion-report.html)
