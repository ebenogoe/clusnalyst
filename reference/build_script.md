# Write an R script that repeats an analysis

Write an R script that repeats an analysis

## Usage

``` r
build_script(settings, source, meta_file = NULL)
```

## Arguments

- settings:

  A settings list (see
  [`default_settings()`](https://ebenogoe.github.io/clusnalyst/reference/default_settings.md)).

- source:

  List describing the data: `demo` (a demo name) or `file`, plus `type`
  and `transposed` for tables.

- meta_file:

  Optional metadata file name.

## Value

A character string of R code.
