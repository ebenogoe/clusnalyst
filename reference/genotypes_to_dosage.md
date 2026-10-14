# Convert letter genotypes to allele dosages

Accepts two-letter calls (`AA`, `AG`), separated calls (`A/G`, `A|G`)
and single IUPAC codes (`A`, `R`, `Y`, ...). For each marker the most
common allele is the reference, so a dosage counts copies of the other
allele: 0 = homozygous for the common allele, 1 = heterozygous, 2 =
homozygous for the other allele. Markers with more than two alleles
count every allele that is not the common one.

## Usage

``` r
genotypes_to_dosage(m)
```

## Arguments

- m:

  Character matrix with samples in rows and markers in columns.

## Value

Numeric matrix of 0, 1, 2 and `NA`, same dimensions as `m`.

## Examples

``` r
genotypes_to_dosage(matrix(c("AA", "AG", "GG", "R", "NN", "A/G"), 3))
#>      [,1] [,2]
#> [1,]    0    1
#> [2,]    1   NA
#> [3,]    2    1
```
