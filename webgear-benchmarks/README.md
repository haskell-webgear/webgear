# WebGear Benchmarking
Some benchmarks comparing webgear against other Haskell web frameworks.

## Users benchmark

### Criterion
This benchmark runs a sequence of PUT, GET, and DELETE operations with criterion. This can be run with the following
command:

```shell
nix develop .#webgear-dev-ghc<GHC-VERSION>
cabal run webgear-benchmarks:users -- --output criterion-report.html
```

### Sample Results

```
Build profile: -w ghc-9.8.1 -O1
benchmarking webgear/500
time                 1.239 ms   (1.236 ms .. 1.241 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.235 ms   (1.231 ms .. 1.245 ms)
std dev              18.66 μs   (8.564 μs .. 34.55 μs)

benchmarking webgear/1000
time                 2.487 ms   (2.470 ms .. 2.508 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.485 ms   (2.479 ms .. 2.493 ms)
std dev              22.19 μs   (16.97 μs .. 34.01 μs)

benchmarking webgear/5000
time                 12.59 ms   (12.53 ms .. 12.65 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.40 ms   (12.35 ms .. 12.44 ms)
std dev              112.6 μs   (91.05 μs .. 144.7 μs)

benchmarking servant/500
time                 6.396 ms   (6.375 ms .. 6.421 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.356 ms   (6.341 ms .. 6.370 ms)
std dev              43.18 μs   (34.93 μs .. 55.56 μs)

benchmarking servant/1000
time                 12.69 ms   (12.61 ms .. 12.78 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.81 ms   (12.77 ms .. 12.85 ms)
std dev              99.71 μs   (81.54 μs .. 129.0 μs)

benchmarking servant/5000
time                 63.94 ms   (63.59 ms .. 64.42 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 63.69 ms   (63.54 ms .. 63.87 ms)
std dev              305.4 μs   (216.9 μs .. 425.3 μs)

benchmarking scotty/500
time                 5.611 ms   (5.598 ms .. 5.623 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.607 ms   (5.598 ms .. 5.617 ms)
std dev              29.13 μs   (22.27 μs .. 45.72 μs)

benchmarking scotty/1000
time                 11.33 ms   (11.28 ms .. 11.39 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.24 ms   (11.19 ms .. 11.28 ms)
std dev              118.6 μs   (94.75 μs .. 144.6 μs)

benchmarking scotty/5000
time                 56.44 ms   (56.11 ms .. 56.79 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 56.48 ms   (56.32 ms .. 56.61 ms)
std dev              275.0 μs   (199.4 μs .. 413.6 μs)
```
