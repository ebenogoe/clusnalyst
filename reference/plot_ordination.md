# Plot an ordination

Plot an ordination

## Usage

``` r
plot_ordination(
  ord,
  groups = NULL,
  group_title = "Group",
  axes = c(1, 2),
  labels = FALSE,
  ellipses = TRUE,
  tooltip = NULL,
  dark = FALSE
)
```

## Arguments

- ord:

  A `cl_ord` from
  [`ordinate()`](https://ebenogoe.github.io/clusnalyst/reference/ordinate.md).

- groups:

  Optional named vector of groups for colour.

- group_title:

  Legend title.

- axes:

  Which two axes to plot.

- labels:

  Show sample names.

- ellipses:

  Draw 95% ellipses for groups with at least four samples.

- tooltip:

  Optional named character vector of hover text.

- dark:

  Colours for a dark background.

## Value

A ggplot object.
