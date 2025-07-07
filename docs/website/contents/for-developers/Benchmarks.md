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
