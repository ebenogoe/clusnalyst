# Read a second matrix for a Mantel test

A square table whose row and column names are the same samples is read
as a distance matrix. Anything else is read as data with
[`read_markers()`](https://ebenogoe.github.io/clusnalyst/reference/read_markers.md)
and turned into distances with the default measure for its type.

## Usage

``` r
read_second_matrix(path, name = basename(path))
```

## Arguments

- path:

  Path to the file.

- name:

  Original file name.

## Value

A `dist` object.
