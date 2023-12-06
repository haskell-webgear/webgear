# WebGear Benchmarking
Some benchmarks comparing webgear against other Haskell web frameworks.

## Users benchmark

### Criterion
This benchmark runs a sequence of PUT, GET, and DELETE operations with criterion. This can be run with the following
command:

```shell
nix develop .#webgear-dev-ghc<GHC-VERSION>
cabal run webgear-benchmarks:users
```

### Sample Results

```
Build profile: -w ghc-9.6.2 -O1
benchmarking webgear/500
time                 2.396 ms   (2.378 ms .. 2.417 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.420 ms   (2.407 ms .. 2.439 ms)
std dev              50.20 μs   (38.91 μs .. 73.25 μs)

benchmarking webgear/1000
time                 4.829 ms   (4.784 ms .. 4.887 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 4.848 ms   (4.820 ms .. 4.904 ms)
std dev              113.0 μs   (68.64 μs .. 208.6 μs)

benchmarking webgear/5000
time                 23.96 ms   (23.75 ms .. 24.31 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 24.26 ms   (24.06 ms .. 24.59 ms)
std dev              594.6 μs   (309.3 μs .. 1.034 ms)

benchmarking servant/500
time                 13.40 ms   (13.30 ms .. 13.56 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 13.54 ms   (13.47 ms .. 13.64 ms)
std dev              211.9 μs   (142.9 μs .. 331.0 μs)

benchmarking servant/1000
time                 26.95 ms   (26.69 ms .. 27.17 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 27.31 ms   (27.12 ms .. 27.65 ms)
std dev              559.1 μs   (289.2 μs .. 937.0 μs)

benchmarking servant/5000
time                 136.4 ms   (135.1 ms .. 137.9 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 136.8 ms   (136.4 ms .. 137.2 ms)
std dev              660.9 μs   (448.8 μs .. 944.8 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking scotty/500
time                 15.66 ms   (15.04 ms .. 16.90 ms)
                     0.985 R²   (0.963 R² .. 1.000 R²)
mean                 15.49 ms   (15.31 ms .. 16.12 ms)
std dev              713.6 μs   (230.4 μs .. 1.411 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking scotty/1000
time                 30.01 ms   (29.79 ms .. 30.26 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 30.49 ms   (30.25 ms .. 31.03 ms)
std dev              724.9 μs   (367.2 μs .. 1.188 ms)

benchmarking scotty/5000
time                 151.0 ms   (149.7 ms .. 152.5 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 151.5 ms   (150.5 ms .. 153.7 ms)
std dev              2.111 ms   (469.2 μs .. 3.353 ms)
variance introduced by outliers: 12% (moderately inflated)
```
