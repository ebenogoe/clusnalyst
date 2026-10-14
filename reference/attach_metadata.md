# Attach metadata to a dataset

Attach metadata to a dataset

## Usage

``` r
attach_metadata(d, meta)
```

## Arguments

- d:

  A `cl_data` object.

- meta:

  A data frame from
  [`read_metadata()`](https://ebenogoe.github.io/clusnalyst/reference/read_metadata.md).

## Value

`d` with `meta` holding one row per sample (in sample order).
