# Demo datasets

Five small datasets for trying the app. `binary`, `snp` and `traits` are
simulated (see `data-raw/make_demos.R` in the source). `dna` is the
woodmouse cytochrome b alignment from the ape package, and `protein` is
the hemoglobin alpha example from the msa package.

## Usage

``` r
demo_info()

clusnalyst_demo(name = c("snp", "binary", "traits", "dna", "protein"))
```

## Arguments

- name:

  One of `demo_info()$name`.

## Value

`demo_info()` returns a data frame describing each dataset.

`clusnalyst_demo()` returns a `cl_data` object, with metadata where the
demo has it.

## Examples

``` r
clusnalyst_demo("snp")
#> <Clusnalyst data>
#>   Type:     SNP genotypes 
#>   Samples:  60 
#>   SNPs: 1000
#>   Metadata columns: population, latitude, longitude 
```
