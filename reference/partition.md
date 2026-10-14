# Split samples into groups

Split samples into groups

## Usage

``` r
partition(
  dist,
  k,
  method = c("tree", "pam", "kmeans"),
  tree = NULL,
  d = NULL,
  seed = 1
)
```

## Arguments

- dist:

  A `dist` object.

- k:

  Number of groups.

- method:

  `"tree"` (cut a hierarchical tree), `"pam"` (partitioning around
  medoids, any distance) or `"kmeans"` (numeric data only).

- tree:

  A hierarchical `cl_tree`, for `method = "tree"`.

- d:

  A `cl_data` object, for `method = "kmeans"`.

- seed:

  Random seed for k-means.

## Value

A named integer vector of group numbers. Groups are numbered in the
order they first appear down the tree when a tree is given.
