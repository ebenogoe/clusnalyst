# Mantel test between two distance matrices

Uses the samples the two matrices share.

## Usage

``` r
mantel_test(d1, d2, permutations = 999, seed = 1)
```

## Arguments

- d1, d2:

  `dist` objects (or square matrices) with sample names.

- permutations:

  Number of permutations.

- seed:

  Random seed.

## Value

A list with `r`, `p`, `n` and `permutations`.
