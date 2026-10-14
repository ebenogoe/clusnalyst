# Plot a tree

Plot a tree

## Usage

``` r
plot_tree(
  tree,
  layout = "rectangular",
  groups = NULL,
  group_title = "Group",
  support = NULL,
  support_min = 0,
  tooltip = NULL,
  label_size = 3.5,
  title = NULL,
  dark = FALSE,
  show_scale = TRUE
)
```

## Arguments

- tree:

  A `cl_tree` or an `ape` `phylo`.

- layout:

  One of `"rectangular"`, `"circular"`, `"fan"`, `"unrooted"` or
  `"slanted"`.

- groups:

  Optional named vector (names = sample names) used to colour the tips.

- group_title:

  Legend title for `groups`.

- support:

  Optional bootstrap support, one value per internal node.

- support_min:

  Only show support at or above this value.

- tooltip:

  Optional named character vector of hover text per tip.

- label_size:

  Size of the tip labels.

- title:

  Plot title.

- dark:

  Colours for a dark background.

- show_scale:

  Draw a branch-length scale bar.

## Value

A ggplot object (interactive with
[`ggiraph::girafe()`](https://davidgohel.github.io/ggiraph/reference/girafe.html)).
