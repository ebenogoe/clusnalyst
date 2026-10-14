# Read a data file into a Clusnalyst dataset

Reads CSV, TSV or Excel tables (first column = sample IDs), VCF, HapMap
and FASTA files. The data type and the table orientation are guessed
when not given.

## Usage

``` r
read_markers(
  path,
  name = basename(path),
  format = NULL,
  type = NULL,
  transpose = NULL
)
```

## Arguments

- path:

  Path to the file.

- name:

  Original file name (used for format detection).

- format:

  File format; guessed with
  [`detect_format()`](https://ebenogoe.github.io/clusnalyst/reference/detect_format.md)
  if `NULL`.

- type:

  Data type, one of
  [`data_types()`](https://ebenogoe.github.io/clusnalyst/reference/data_types.md);
  guessed if `NULL`.

- transpose:

  `TRUE` if samples are in columns and markers in rows. Guessed if
  `NULL`.

## Value

A `cl_data` object.

## Examples

``` r
f <- system.file("extdata", "demo_binary.csv", package = "clusnalyst")
read_markers(f)
#> <Clusnalyst data>
#>   Type:     Dominant or binary markers (0/1) 
#>   Samples:  30 
#>   Markers: 60
```
