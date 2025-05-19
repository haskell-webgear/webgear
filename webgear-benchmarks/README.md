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
time                 66.97 μs   (66.73 μs .. 67.11 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 66.65 μs   (66.49 μs .. 66.88 μs)
std dev              636.6 ns   (501.5 ns .. 903.5 ns)

benchmarking hello-world/Servant
time                 58.90 μs   (58.41 μs .. 59.65 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 59.09 μs   (58.73 μs .. 59.75 μs)
std dev              1.568 μs   (1.039 μs .. 2.455 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking hello-world/WebGear
time                 11.78 μs   (11.65 μs .. 11.93 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 11.88 μs   (11.79 μs .. 11.97 μs)
std dev              311.3 ns   (221.9 ns .. 464.1 ns)
variance introduced by outliers: 29% (moderately inflated)

benchmarking path-variable/Scotty
time                 198.7 μs   (197.0 μs .. 201.3 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 200.3 μs   (198.8 μs .. 202.3 μs)
std dev              6.070 μs   (4.618 μs .. 8.510 μs)
variance introduced by outliers: 26% (moderately inflated)

benchmarking path-variable/Servant
time                 133.9 μs   (132.9 μs .. 134.9 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 132.9 μs   (132.0 μs .. 134.3 μs)
std dev              3.750 μs   (3.073 μs .. 5.138 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking path-variable/WebGear
time                 48.77 μs   (48.33 μs .. 49.18 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 47.74 μs   (47.40 μs .. 48.26 μs)
std dev              1.391 μs   (1.110 μs .. 1.826 μs)
variance introduced by outliers: 29% (moderately inflated)

benchmarking query-param/Scotty
time                 209.2 μs   (206.7 μs .. 211.5 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 207.2 μs   (206.2 μs .. 208.8 μs)
std dev              4.390 μs   (3.255 μs .. 6.074 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking query-param/Servant
time                 138.7 μs   (137.2 μs .. 140.0 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 137.0 μs   (136.3 μs .. 137.9 μs)
std dev              2.781 μs   (2.111 μs .. 4.190 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking query-param/WebGear
time                 94.20 μs   (93.90 μs .. 94.47 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 94.46 μs   (93.96 μs .. 95.60 μs)
std dev              2.306 μs   (973.1 ns .. 3.901 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking header/Scotty
time                 98.79 μs   (98.66 μs .. 98.91 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 99.09 μs   (98.90 μs .. 99.32 μs)
std dev              717.2 ns   (572.9 ns .. 922.6 ns)

benchmarking header/Servant
time                 77.22 μs   (76.84 μs .. 77.68 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 77.07 μs   (76.85 μs .. 77.28 μs)
std dev              738.9 ns   (642.1 ns .. 917.5 ns)

benchmarking header/WebGear
time                 43.20 μs   (43.01 μs .. 43.47 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 43.60 μs   (43.48 μs .. 43.70 μs)
std dev              389.5 ns   (291.7 ns .. 482.8 ns)

benchmarking upload/Scotty
time                 167.7 μs   (165.5 μs .. 169.2 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 163.5 μs   (162.0 μs .. 165.3 μs)
std dev              5.658 μs   (4.888 μs .. 7.063 μs)
variance introduced by outliers: 32% (moderately inflated)

benchmarking upload/Servant
time                 170.7 μs   (165.3 μs .. 174.8 μs)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 165.6 μs   (164.5 μs .. 167.7 μs)
std dev              4.882 μs   (3.020 μs .. 7.331 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking upload/WebGear
time                 213.6 μs   (203.4 μs .. 226.6 μs)
                     0.984 R²   (0.981 R² .. 0.991 R²)
mean                 223.0 μs   (217.0 μs .. 229.2 μs)
std dev              19.40 μs   (16.68 μs .. 22.43 μs)
variance introduced by outliers: 74% (severely inflated)

benchmarking download/Scotty
time                 1.458 ms   (1.446 ms .. 1.466 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.481 ms   (1.470 ms .. 1.496 ms)
std dev              46.19 μs   (34.99 μs .. 58.78 μs)
variance introduced by outliers: 18% (moderately inflated)

benchmarking download/Servant
time                 1.501 ms   (1.484 ms .. 1.513 ms)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 1.447 ms   (1.438 ms .. 1.458 ms)
std dev              33.48 μs   (27.18 μs .. 39.17 μs)
variance introduced by outliers: 12% (moderately inflated)

benchmarking download/WebGear
time                 1.601 ms   (1.455 ms .. 1.728 ms)
                     0.965 R²   (0.950 R² .. 0.992 R²)
mean                 1.414 ms   (1.376 ms .. 1.490 ms)
std dev              161.4 μs   (102.4 μs .. 243.4 μs)
variance introduced by outliers: 77% (severely inflated)
```
