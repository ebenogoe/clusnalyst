# Bootstrap support for a tree

Resamples the markers, traits or alignment sites with replacement,
recomputes the distance matrix and the tree for each replicate, and
reports how often each group in the original tree is found again
(Felsenstein 1985). For rooted trees the groups are clades; for unrooted
NJ and BIONJ trees they are bipartitions.

## Usage

``` r
bootstrap_tree(
  d,
  tree,
  distance = NULL,
  B = 100,
  seed = 1,
  sqrt_binary = FALSE,
  scale = TRUE,
  progress = NULL
)
```

## Arguments

- d:

  The `cl_data` the tree was built from.

- tree:

  A `cl_tree` from
  [`build_tree()`](https://ebenogoe.github.io/clusnalyst/reference/build_tree.md).

- distance:

  Distance method used for the tree.

- B:

  Number of bootstrap replicates.

- seed:

  Random seed, so the run can be repeated exactly.

- sqrt_binary, scale:

  Passed to
  [`compute_distance()`](https://ebenogoe.github.io/clusnalyst/reference/compute_distance.md).

- progress:

  Optional function called with the share done (0 to 1).

## Value

A list with `support` (percent, one value per internal node of
`tree$phylo`), `trees` (the replicate trees), `B`, `failed` and `seed`.
