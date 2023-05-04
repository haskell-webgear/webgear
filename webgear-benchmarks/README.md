# WebGear Benchmarking
Some benchmarks comparing webgear against other Haskell web frameworks.

## Users benchmark

### Criterion
This benchmark runs a sequence of PUT, GET, and DELETE operations with criterion. This can be run with the following
command:

```shell
nix develop .#webgear-dev-ghc<GHC-VERSION>
cabal run users
```

### Sample Results

```
Build profile: -w ghc-9.4.4 -O1
benchmarking webgear/500
time                 13.05 ms   (12.92 ms .. 13.20 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 13.43 ms   (13.29 ms .. 13.71 ms)
std dev              541.6 μs   (277.3 μs .. 888.8 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking webgear/1000
time                 26.37 ms   (26.18 ms .. 26.59 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 26.59 ms   (26.47 ms .. 27.03 ms)
std dev              415.5 μs   (130.2 μs .. 792.1 μs)

benchmarking webgear/5000
time                 131.1 ms   (130.9 ms .. 131.4 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 131.1 ms   (130.9 ms .. 131.2 ms)
std dev              219.1 μs   (128.0 μs .. 331.0 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking servant/500
time                 9.332 ms   (9.233 ms .. 9.470 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 9.425 ms   (9.352 ms .. 9.538 ms)
std dev              249.2 μs   (142.0 μs .. 371.1 μs)

benchmarking servant/1000
time                 18.61 ms   (18.48 ms .. 18.81 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 18.68 ms   (18.55 ms .. 18.85 ms)
std dev              362.3 μs   (260.6 μs .. 520.6 μs)

benchmarking servant/5000
time                 93.66 ms   (91.64 ms .. 95.92 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 95.10 ms   (94.33 ms .. 96.32 ms)
std dev              1.581 ms   (797.9 μs .. 2.527 ms)

benchmarking scotty/500
time                 13.24 ms   (13.19 ms .. 13.32 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 13.39 ms   (13.35 ms .. 13.46 ms)
std dev              141.8 μs   (98.32 μs .. 231.3 μs)

benchmarking scotty/1000
time                 26.20 ms   (25.23 ms .. 26.83 ms)
                     0.994 R²   (0.985 R² .. 0.999 R²)
mean                 27.39 ms   (26.94 ms .. 28.46 ms)
std dev              1.458 ms   (689.0 μs .. 2.542 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking scotty/5000
time                 133.3 ms   (132.2 ms .. 134.6 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 134.1 ms   (133.3 ms .. 135.6 ms)
std dev              1.682 ms   (821.6 μs .. 2.582 ms)
variance introduced by outliers: 11% (moderately inflated)
```
