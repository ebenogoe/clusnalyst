# Compute a distance matrix

Missing values are handled pairwise: each pair of samples is compared on
the markers (or sites) both of them have.

## Usage

``` r
compute_distance(d, method = NULL, sqrt_binary = FALSE, scale = TRUE)
```

## Arguments

- d:

  A `cl_data` object.

- method:

  A method from
  [`distance_choices()`](https://ebenogoe.github.io/clusnalyst/reference/distance_choices.md);
  the default for the data type if `NULL`.

- sqrt_binary:

  Use sqrt(1 - S) for binary measures.

- scale:

  Scale numeric traits to unit variance before Euclidean or Manhattan
  distances.

## Value

A `dist` object with a `method` attribute.

## Details

Binary measures are returned as 1 minus the similarity. Set
`sqrt_binary = TRUE` for the square root form that
[`ade4::dist.binary()`](https://adeverse.github.io/ade4/reference/dist.binary.html)
returns, which is Euclidean. For biallelic SNPs, IBS distance equals
Prevosti's distance.

## Examples

``` r
d <- clusnalyst_demo("binary")
dm <- compute_distance(d, "jaccard")
```
