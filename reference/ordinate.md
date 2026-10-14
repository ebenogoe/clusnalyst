# Principal coordinates or principal components

PCoA (classical multidimensional scaling) works from any distance
matrix. PCA needs numeric data; missing values are replaced by the
column mean, and traits (not markers) are scaled to unit variance.

## Usage

``` r
ordinate(dist = NULL, d = NULL, method = c("pcoa", "pca"), n_axes = 5)
```

## Arguments

- dist:

  A `dist` object (for PCoA).

- d:

  A `cl_data` object (for PCA).

- method:

  `"pcoa"` or `"pca"`.

- n_axes:

  Number of axes to keep.

## Value

A `cl_ord` list with `scores`, `variance` (percent for every axis) and
`method`.
