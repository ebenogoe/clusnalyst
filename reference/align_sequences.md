# Align sequences with the msa package

Align sequences with the msa package

## Usage

``` r
align_sequences(d, method = c("ClustalOmega", "ClustalW", "Muscle"))
```

## Arguments

- d:

  A `cl_data` object of type `"dna"` or `"protein"`.

- method:

  `"ClustalOmega"`, `"ClustalW"` or `"Muscle"`.

## Value

The same object with an aligned matrix in `x`.
