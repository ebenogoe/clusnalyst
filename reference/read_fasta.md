# Read a FASTA file

Read a FASTA file

## Usage

``` r
read_fasta(path, type = NULL)
```

## Arguments

- path:

  Path to the file.

- type:

  `"dna"` or `"protein"`; guessed from the letters if `NULL`.

## Value

A `cl_data` object of type `"dna"` or `"protein"`.
