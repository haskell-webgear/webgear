# WebGear Benchmarking
Some benchmarks comparing webgear against other Haskell web frameworks.

## Users benchmark

### Criterion
This benchmark runs a sequence of PUT, GET, and DELETE operations with criterion. This can be run with the following
command:

```
stack bench --ba "--time-limit 15"
```

### Results

```
webgear-benchmarks> benchmarks
Running 1 benchmarks...
Benchmark bench-users: RUNNING...
benchmarking webgear
time                 23.67 ms   (22.33 ms .. 25.24 ms)
                     0.981 R²   (0.971 R² .. 0.990 R²)
mean                 26.11 ms   (25.50 ms .. 26.45 ms)
std dev              1.484 ms   (1.023 ms .. 1.934 ms)
variance introduced by outliers: 29% (moderately inflated)

benchmarking servant
time                 24.06 ms   (23.54 ms .. 24.34 ms)
                     0.994 R²   (0.984 R² .. 1.000 R²)
mean                 23.97 ms   (23.60 ms .. 24.25 ms)
std dev              963.9 μs   (493.0 μs .. 1.479 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking scotty
time                 28.91 ms   (27.54 ms .. 30.82 ms)
                     0.983 R²   (0.974 R² .. 0.990 R²)
mean                 33.29 ms   (32.42 ms .. 33.99 ms)
std dev              2.300 ms   (1.799 ms .. 2.793 ms)
variance introduced by outliers: 36% (moderately inflated)

Benchmark bench-users: FINISH
```

These benchmarks were run on a MacBook Pro 2019, Intel Core i7, 6-core 2.6 GHz, 32 GB RAM.
