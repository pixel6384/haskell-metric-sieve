# Haskell Metric Sieve

A lightweight streaming tool for filtering and analyzing system metrics from stdin.

## Usage

Compile with cabal:
```bash
cabal build
```

Run by piping logs into the tool:
```bash
cat metrics.log | cabal run metric-sieve -- cpu_usage 50.0
```

This will output all lines where `cpu_usage` is greater than 50.0.