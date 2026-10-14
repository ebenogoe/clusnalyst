# Filter markers, traits or sequences

Markers (or traits) are filtered first, then samples are checked against
the markers that are left.

## Usage

``` r
filter_data(
  d,
  max_marker_missing = 0.2,
  min_maf = 0,
  drop_monomorphic = TRUE,
  max_sample_missing = 0.5,
  drop_empty_sites = TRUE
)
```

## Arguments

- d:

  A `cl_data` object.

- max_marker_missing:

  Largest share of missing values allowed per marker or trait (0 to 1).

- min_maf:

  Smallest minor allele (or band) frequency kept (marker data only).

- drop_monomorphic:

  Drop markers or traits that do not vary.

- max_sample_missing:

  Largest share of missing values allowed per sample.

- drop_empty_sites:

  For aligned sequences, drop sites that are gaps in every sequence.

## Value

The filtered `cl_data`, with a `filter` element recording what was
removed.
