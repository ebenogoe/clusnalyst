# Distances from numeric metadata columns

Useful for a Mantel test of genetic distance against geography (for
example latitude and longitude) or any measured covariates.

## Usage

``` r
metadata_distance(meta, columns, scale = FALSE)
```

## Arguments

- meta:

  A data frame with sample names as row names.

- columns:

  Numeric columns to use.

- scale:

  Scale the columns first.

## Value

A Euclidean `dist` object.
