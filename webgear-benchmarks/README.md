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
time                 1.234 ms   (1.230 ms .. 1.239 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.233 ms   (1.231 ms .. 1.234 ms)
std dev              4.853 μs   (3.704 μs .. 6.524 μs)

benchmarking webgear/1000
time                 2.512 ms   (2.499 ms .. 2.522 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.489 ms   (2.484 ms .. 2.495 ms)
std dev              17.53 μs   (14.21 μs .. 21.67 μs)

benchmarking webgear/5000
time                 12.54 ms   (12.45 ms .. 12.65 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.48 ms   (12.44 ms .. 12.53 ms)
std dev              110.2 μs   (90.59 μs .. 140.0 μs)

benchmarking servant/500
time                 6.254 ms   (6.226 ms .. 6.286 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.225 ms   (6.206 ms .. 6.240 ms)
std dev              52.75 μs   (44.65 μs .. 65.99 μs)

benchmarking servant/1000
time                 12.36 ms   (12.30 ms .. 12.41 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.36 ms   (12.33 ms .. 12.39 ms)
std dev              70.20 μs   (58.27 μs .. 83.55 μs)

benchmarking servant/5000
time                 62.05 ms   (61.64 ms .. 62.45 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 61.87 ms   (61.65 ms .. 62.04 ms)
std dev              357.8 μs   (239.0 μs .. 530.3 μs)

benchmarking scotty/500
time                 5.598 ms   (5.578 ms .. 5.617 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.602 ms   (5.590 ms .. 5.615 ms)
std dev              37.64 μs   (28.96 μs .. 49.44 μs)

benchmarking scotty/1000
time                 11.11 ms   (11.08 ms .. 11.16 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.15 ms   (11.11 ms .. 11.18 ms)
std dev              97.38 μs   (77.12 μs .. 122.4 μs)

benchmarking scotty/5000
time                 55.49 ms   (54.87 ms .. 55.92 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 55.90 ms   (55.68 ms .. 56.20 ms)
std dev              482.0 μs   (350.9 μs .. 697.1 μs)
```
