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
Build profile: -w ghc-9.10.1 -O1

benchmarking webgear/500
time                 1.213 ms   (1.210 ms .. 1.216 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.222 ms   (1.219 ms .. 1.225 ms)
std dev              10.66 μs   (9.564 μs .. 12.25 μs)

benchmarking webgear/1000
time                 2.515 ms   (2.507 ms .. 2.521 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.504 ms   (2.499 ms .. 2.508 ms)
std dev              14.11 μs   (11.62 μs .. 17.13 μs)

benchmarking webgear/5000
time                 12.48 ms   (12.41 ms .. 12.55 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.30 ms   (12.25 ms .. 12.34 ms)
std dev              118.4 μs   (98.43 μs .. 141.5 μs)

benchmarking servant/500
time                 6.217 ms   (6.180 ms .. 6.268 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.289 ms   (6.270 ms .. 6.308 ms)
std dev              57.30 μs   (47.88 μs .. 69.36 μs)

benchmarking servant/1000
time                 12.37 ms   (12.31 ms .. 12.43 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.35 ms   (12.33 ms .. 12.37 ms)
std dev              67.69 μs   (55.23 μs .. 82.86 μs)

benchmarking servant/5000
time                 62.19 ms   (62.01 ms .. 62.39 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 61.87 ms   (61.59 ms .. 62.04 ms)
std dev              391.9 μs   (257.7 μs .. 609.9 μs)

benchmarking scotty/500
time                 5.553 ms   (5.536 ms .. 5.572 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.529 ms   (5.514 ms .. 5.541 ms)
std dev              42.81 μs   (34.36 μs .. 54.02 μs)

benchmarking scotty/1000
time                 10.66 ms   (10.58 ms .. 10.74 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.86 ms   (10.81 ms .. 10.91 ms)
std dev              127.0 μs   (106.7 μs .. 157.8 μs)

benchmarking scotty/5000
time                 55.63 ms   (55.02 ms .. 56.14 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 54.85 ms   (54.62 ms .. 55.17 ms)
std dev              492.0 μs   (372.6 μs .. 619.0 μs)
```
