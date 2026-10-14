# Run a whole analysis from settings

Run a whole analysis from settings

## Usage

``` r
run_analysis(d, settings = default_settings(d), progress = NULL)
```

## Arguments

- d:

  A `cl_data` object as read (before filtering).

- settings:

  A list like
  [`default_settings()`](https://ebenogoe.github.io/clusnalyst/reference/default_settings.md).
  Steps whose entry is `NULL` are skipped.

- progress:

  Optional function called with a message at each step.

## Value

A list with `data`, `dist`, `tree`, `boot`, `kres`, `groups` and `ord`.

## Examples

``` r
# \donttest{
res <- run_analysis(clusnalyst_demo("binary"))
res$tree
#> <Clusnalyst tree> UPGMA (average linkage) with 30 tips
# }
```
