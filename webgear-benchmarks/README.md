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
time                 1.471 ms   (1.469 ms .. 1.474 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.467 ms   (1.466 ms .. 1.469 ms)
std dev              4.246 μs   (3.379 μs .. 5.759 μs)

benchmarking webgear/1000
time                 2.942 ms   (2.938 ms .. 2.947 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.918 ms   (2.914 ms .. 2.924 ms)
std dev              17.13 μs   (15.27 μs .. 20.36 μs)

benchmarking webgear/5000
time                 14.72 ms   (14.61 ms .. 14.88 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 14.63 ms   (14.60 ms .. 14.70 ms)
std dev              121.7 μs   (62.96 μs .. 216.3 μs)

benchmarking servant/500
time                 7.188 ms   (7.158 ms .. 7.220 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 7.187 ms   (7.170 ms .. 7.212 ms)
std dev              62.01 μs   (50.88 μs .. 78.39 μs)

benchmarking servant/1000
time                 14.40 ms   (14.21 ms .. 14.56 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 14.51 ms   (14.43 ms .. 14.82 ms)
std dev              345.9 μs   (90.11 μs .. 675.3 μs)

benchmarking servant/5000
time                 71.61 ms   (70.92 ms .. 72.25 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 72.41 ms   (72.06 ms .. 72.79 ms)
std dev              662.9 μs   (507.6 μs .. 884.0 μs)

benchmarking scotty/500
time                 7.941 ms   (7.900 ms .. 7.976 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 7.999 ms   (7.977 ms .. 8.017 ms)
std dev              54.05 μs   (44.34 μs .. 72.74 μs)

benchmarking scotty/1000
time                 15.96 ms   (15.80 ms .. 16.18 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 16.07 ms   (16.01 ms .. 16.17 ms)
std dev              189.6 μs   (135.3 μs .. 294.9 μs)

benchmarking scotty/5000
time                 78.93 ms   (77.49 ms .. 79.69 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 79.69 ms   (79.10 ms .. 80.45 ms)
std dev              1.128 ms   (749.1 μs .. 1.632 ms)
```
