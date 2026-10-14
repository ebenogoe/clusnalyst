# Read a sample metadata table

Read a sample metadata table

## Usage

``` r
read_metadata(path, name = basename(path))
```

## Arguments

- path:

  Path to a CSV, TSV or Excel file whose first column holds sample IDs.

- name:

  Original file name.

## Value

A data frame with sample IDs as row names.
