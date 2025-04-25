# WebGear Benchmarking
Some benchmarks comparing webgear against other Haskell web frameworks.

To run this benchmark:

```shell
nix develop .#webgear-dev-ghc<GHC-VERSION>
cabal run webgear-benchmarks:benchmarks -- --output criterion-report.html
```

### Sample Results

```
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.12.1

$ cabal run webgear-benchmarks:benchmarks -- --output criterion-report.html
Configuration is affected by the following files:
- cabal.project
benchmarking hello-world/Scotty
time                 65.79 μs   (65.47 μs .. 66.14 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 65.93 μs   (65.77 μs .. 66.12 μs)
std dev              584.2 ns   (433.9 ns .. 831.1 ns)

benchmarking hello-world/Servant
time                 56.86 μs   (56.70 μs .. 57.09 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 57.49 μs   (57.33 μs .. 57.67 μs)
std dev              587.2 ns   (505.8 ns .. 754.6 ns)

benchmarking hello-world/WebGear
time                 11.29 μs   (11.27 μs .. 11.31 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.25 μs   (11.24 μs .. 11.26 μs)
std dev              27.80 ns   (21.30 ns .. 37.21 ns)

benchmarking path-variable/Scotty
time                 194.7 μs   (194.3 μs .. 194.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 193.8 μs   (193.5 μs .. 194.0 μs)
std dev              862.6 ns   (758.3 ns .. 1.040 μs)

benchmarking path-variable/Servant
time                 128.5 μs   (128.3 μs .. 128.6 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 128.0 μs   (127.9 μs .. 128.2 μs)
std dev              473.4 ns   (421.2 ns .. 542.0 ns)

benchmarking path-variable/WebGear
time                 48.30 μs   (48.24 μs .. 48.35 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 48.02 μs   (47.93 μs .. 48.09 μs)
std dev              259.9 ns   (220.2 ns .. 358.1 ns)

benchmarking query-param/Scotty
time                 202.2 μs   (201.8 μs .. 202.7 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 201.0 μs   (200.6 μs .. 201.4 μs)
std dev              1.261 μs   (1.103 μs .. 1.465 μs)

benchmarking query-param/Servant
time                 134.0 μs   (133.8 μs .. 134.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 133.2 μs   (133.0 μs .. 133.5 μs)
std dev              758.3 ns   (680.9 ns .. 866.8 ns)

benchmarking query-param/WebGear
time                 98.46 μs   (98.18 μs .. 98.74 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 98.17 μs   (98.01 μs .. 98.38 μs)
std dev              614.3 ns   (459.1 ns .. 804.3 ns)

benchmarking header/Scotty
time                 99.17 μs   (98.83 μs .. 99.42 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 98.88 μs   (98.71 μs .. 99.08 μs)
std dev              620.4 ns   (523.5 ns .. 862.9 ns)

benchmarking header/Servant
time                 78.02 μs   (77.85 μs .. 78.21 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 77.92 μs   (77.79 μs .. 78.08 μs)
std dev              462.0 ns   (348.7 ns .. 601.4 ns)

benchmarking header/WebGear
time                 42.61 μs   (42.52 μs .. 42.69 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 42.77 μs   (42.66 μs .. 42.91 μs)
std dev              407.4 ns   (321.7 ns .. 562.2 ns)

benchmarking upload/Scotty
time                 163.4 μs   (160.9 μs .. 167.0 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 161.5 μs   (160.9 μs .. 162.8 μs)
std dev              2.920 μs   (1.724 μs .. 4.782 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking upload/Servant
time                 170.1 μs   (166.5 μs .. 174.7 μs)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 176.2 μs   (173.8 μs .. 179.1 μs)
std dev              9.143 μs   (7.624 μs .. 11.40 μs)
variance introduced by outliers: 51% (severely inflated)

benchmarking upload/WebGear
time                 216.9 μs   (208.5 μs .. 227.4 μs)
                     0.991 R²   (0.987 R² .. 0.997 R²)
mean                 227.7 μs   (223.6 μs .. 230.6 μs)
std dev              11.65 μs   (7.990 μs .. 16.32 μs)
variance introduced by outliers: 50% (moderately inflated)

benchmarking download/Scotty
time                 1.381 ms   (1.375 ms .. 1.385 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.374 ms   (1.372 ms .. 1.379 ms)
std dev              11.05 μs   (8.855 μs .. 14.75 μs)

benchmarking download/Servant
time                 1.398 ms   (1.391 ms .. 1.404 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.374 ms   (1.368 ms .. 1.379 ms)
std dev              18.74 μs   (16.37 μs .. 22.90 μs)

benchmarking download/WebGear
time                 1.324 ms   (1.284 ms .. 1.362 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 1.276 ms   (1.268 ms .. 1.289 ms)
std dev              33.94 μs   (21.49 μs .. 56.67 μs)
variance introduced by outliers: 14% (moderately inflated)
```
