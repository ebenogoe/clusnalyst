# Quality summary of a dataset

For marker data: missing data per marker and per sample, minor allele
(or band) frequency, heterozygosity and monomorphic markers. For traits:
missing data and spread per trait. For sequences: lengths, gaps and
variable sites.

## Usage

``` r
qc_summary(d)
```

## Arguments

- d:

  A `cl_data` object.

## Value

A list with `markers` and `samples` data frames and totals.
