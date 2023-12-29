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
time                 2.643 ms   (2.628 ms .. 2.662 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.679 ms   (2.662 ms .. 2.703 ms)
std dev              62.23 μs   (43.60 μs .. 91.04 μs)

benchmarking webgear/1000
time                 5.323 ms   (5.291 ms .. 5.370 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 5.344 ms   (5.323 ms .. 5.373 ms)
std dev              75.56 μs   (54.20 μs .. 99.89 μs)

benchmarking webgear/5000
time                 26.50 ms   (26.22 ms .. 26.78 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 26.74 ms   (26.61 ms .. 26.93 ms)
std dev              342.2 μs   (219.5 μs .. 520.5 μs)

benchmarking servant/500
time                 13.28 ms   (13.16 ms .. 13.38 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 13.39 ms   (13.32 ms .. 13.46 ms)
std dev              187.0 μs   (139.6 μs .. 250.8 μs)

benchmarking servant/1000
time                 26.42 ms   (26.20 ms .. 26.63 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 26.58 ms   (26.45 ms .. 26.83 ms)
std dev              357.5 μs   (163.4 μs .. 580.0 μs)

benchmarking servant/5000
time                 132.2 ms   (131.5 ms .. 133.1 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 132.7 ms   (132.3 ms .. 133.4 ms)
std dev              900.6 μs   (502.2 μs .. 1.355 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking scotty/500
time                 14.83 ms   (14.71 ms .. 14.93 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 14.89 ms   (14.81 ms .. 15.02 ms)
std dev              243.8 μs   (162.6 μs .. 370.9 μs)

benchmarking scotty/1000
time                 29.64 ms   (29.25 ms .. 30.08 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 29.91 ms   (29.67 ms .. 30.74 ms)
std dev              865.6 μs   (201.4 μs .. 1.612 ms)

benchmarking scotty/5000
time                 149.4 ms   (148.5 ms .. 150.2 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 148.6 ms   (147.8 ms .. 149.0 ms)
std dev              855.5 μs   (378.7 μs .. 1.294 ms)
variance introduced by outliers: 12% (moderately inflated)
```
