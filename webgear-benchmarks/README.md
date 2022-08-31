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
Build profile: -w ghc-9.0.2 -O1
In order, the following will be built (use -v for more details):
 - webgear-benchmarks-1.0.4 (exe:users) (file README.md changed)
Preprocessing executable 'users' for webgear-benchmarks-1.0.4..
Building executable 'users' for webgear-benchmarks-1.0.4..
benchmarking webgear/500
time                 8.388 ms   (8.159 ms .. 8.754 ms)
                     0.974 R²   (0.921 R² .. 0.998 R²)
mean                 8.522 ms   (8.334 ms .. 9.024 ms)
std dev              773.3 μs   (373.6 μs .. 1.426 ms)
variance introduced by outliers: 51% (severely inflated)

benchmarking webgear/1000
time                 16.88 ms   (16.56 ms .. 17.24 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 17.30 ms   (17.13 ms .. 17.58 ms)
std dev              529.1 μs   (366.9 μs .. 795.7 μs)

benchmarking webgear/5000
time                 88.47 ms   (86.15 ms .. 92.22 ms)
                     0.995 R²   (0.983 R² .. 1.000 R²)
mean                 87.48 ms   (86.17 ms .. 90.66 ms)
std dev              3.272 ms   (1.291 ms .. 5.392 ms)

benchmarking servant/500
time                 9.350 ms   (9.188 ms .. 9.487 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 9.569 ms   (9.498 ms .. 9.701 ms)
std dev              251.7 μs   (160.4 μs .. 392.9 μs)

benchmarking servant/1000
time                 19.01 ms   (18.86 ms .. 19.16 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 19.09 ms   (18.98 ms .. 19.27 ms)
std dev              322.7 μs   (186.3 μs .. 551.0 μs)

benchmarking servant/5000
time                 95.47 ms   (93.92 ms .. 97.35 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 95.16 ms   (94.46 ms .. 96.06 ms)
std dev              1.258 ms   (789.2 μs .. 1.913 ms)

benchmarking scotty/500
time                 13.23 ms   (13.08 ms .. 13.37 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 13.30 ms   (13.24 ms .. 13.40 ms)
std dev              205.2 μs   (139.9 μs .. 316.2 μs)

benchmarking scotty/1000
time                 27.12 ms   (26.21 ms .. 28.57 ms)
                     0.995 R²   (0.989 R² .. 1.000 R²)
mean                 26.77 ms   (26.55 ms .. 27.35 ms)
std dev              728.5 μs   (342.2 μs .. 1.272 ms)

benchmarking scotty/5000
time                 131.1 ms   (126.4 ms .. 138.6 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 132.8 ms   (131.5 ms .. 134.9 ms)
std dev              2.602 ms   (937.7 μs .. 3.331 ms)
variance introduced by outliers: 11% (moderately inflated)
```
