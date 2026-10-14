# Build a tree from a distance matrix

Hierarchical methods give a rooted, ultrametric tree. NJ and BIONJ give
an unrooted tree; give `outgroup` to root it. Negative branch lengths
from NJ are set to zero.

## Usage

``` r
build_tree(dist, method = "upgma", outgroup = NULL)
```

## Arguments

- dist:

  A `dist` object.

- method:

  A method from
  [`tree_choices()`](https://ebenogoe.github.io/clusnalyst/reference/tree_choices.md).

- outgroup:

  Optional sample name(s) to root an NJ or BIONJ tree on.

## Value

A `cl_tree` list with `phylo` (an `ape` tree), `hclust` (for
hierarchical methods), `method` and `rooted`.

## Examples

``` r
d <- clusnalyst_demo("binary")
tr <- build_tree(compute_distance(d), "upgma")
```
