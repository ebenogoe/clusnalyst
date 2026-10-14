# Choose the number of clusters

For each k from 2 to `k_max`, splits the samples into k groups and
records the average silhouette width (Rousseeuw 1987), the within-group
sum of squares (for an elbow plot) and, optionally, the gap statistic
(Tibshirani et al. 2001). Hierarchical trees are cut with
[`stats::cutree()`](https://rdrr.io/r/stats/cutree.html); NJ and BIONJ
trees cannot be cut that way, so partitioning around medoids (PAM) is
used instead.

## Usage

``` r
optimal_k(dist, tree = NULL, k_max = 10, gap = FALSE, gap_B = 50, seed = 1)
```

## Arguments

- dist:

  A `dist` object.

- tree:

  Optional `cl_tree`. If it is hierarchical, its cuts are used.

- k_max:

  Largest k to try.

- gap:

  Also compute the gap statistic (slower).

- gap_B:

  Reference datasets for the gap statistic.

- seed:

  Random seed for the gap statistic.

## Value

A list with a `table` (one row per k), `best_silhouette`, `best_gap` and
`clustering` (how the groups were formed).
