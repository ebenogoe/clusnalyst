# Write a self-contained HTML report

Includes whichever results are present: data summary, quality check,
distances, tree, bootstrap, cluster number, groups and ordination, plus
the settings and software versions. Figures are embedded as SVG, so the
file can be opened anywhere and needs no other files.

## Usage

``` r
build_report(
  res,
  settings = list(),
  path,
  title = "Clusnalyst report",
  tree_layout = "rectangular"
)
```

## Arguments

- res:

  A list with any of `raw`, `data`, `dist`, `tree`, `boot`, `kres`,
  `groups`, `ord`, `mantel`, as produced by the app or by
  [`run_analysis()`](https://ebenogoe.github.io/clusnalyst/reference/run_analysis.md)
  (add `raw` for the unfiltered data).

- settings:

  The settings list.

- path:

  Output file.

- title:

  Report title.

- tree_layout:

  Tree layout for the figure.

## Value

`path`, invisibly.
