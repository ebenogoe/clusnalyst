# Plot quality-check summaries

Plot quality-check summaries

## Usage

``` r
plot_qc(qc, dark = FALSE, thresholds = NULL)
```

## Arguments

- qc:

  Result of
  [`qc_summary()`](https://ebenogoe.github.io/clusnalyst/reference/qc_summary.md).

- dark:

  Colours for a dark background.

- thresholds:

  Optional named list with `marker_missing` and `maf` cut-offs to draw.

## Value

A ggplot object.
