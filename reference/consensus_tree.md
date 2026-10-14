# Majority-rule consensus of bootstrap trees

Majority-rule consensus of bootstrap trees

## Usage

``` r
consensus_tree(boot, p = 0.5)
```

## Arguments

- boot:

  Result of
  [`bootstrap_tree()`](https://ebenogoe.github.io/clusnalyst/reference/bootstrap_tree.md).

- p:

  Share of trees a group must appear in (0.5 = majority rule).

## Value

An `ape` tree.
