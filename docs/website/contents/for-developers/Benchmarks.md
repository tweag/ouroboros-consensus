# Consensus benchmarks

We are in the process of adding component level microbenchmarks for Consensus.

We check for regressions in performance on CI.

## Mempool Benchmark

We started with microbenchmarks for adding transactions to the mempool. The
mempool benchmarks can be run using the following command.

```sh
cabal new-run ouroboros-consensus:mempool-bench
```

## ChainSync Client Benchmark

During the work on Ouroboros Genesis, we added a ChainSync benchmark which uses test blocks:

```sh
cabal new-run ouroboros-consensus:ChainSync-client-bench -- 10 10
```

## PerasCertDB Benchmark

We have a microbenchmark for the boosted chain fragment weight calculation, which could be run as follows:

```sh
cabal run ouroboros-consensus:PerasCertDB-bench --output bench.html
```

`criterion` allows fitting linear regressions onto the the data emitted by the GHC runtime system, for example:

```sh
cabal run ouroboros-consensus:PerasCertDB-bench -- +RTS -T -RTS --output bench.html --regress allocated:iters
```

See [Criterion.Measurement.Types.measureAccessors_](https://github.com/haskell/criterion/blob/ee9d65a71bc9c830989701a50d5eec991a49699b/criterion-measurement/src/Criterion/Measurement/Types.hs#L179) for other values of the `--regress` option.
