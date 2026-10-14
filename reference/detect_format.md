# Guess the format of a data file

Looks at the file extension first and then at the first lines of the
file.

## Usage

``` r
detect_format(path, name = basename(path))
```

## Arguments

- path:

  Path to the file.

- name:

  Original file name, if `path` is a temporary upload.

## Value

One of `"csv"`, `"tsv"`, `"excel"`, `"vcf"`, `"hapmap"` or `"fasta"`.
