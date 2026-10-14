# clusnalyst: Point-and-Click Cluster Analysis of Molecular and Trait Data

A 'shiny' application and a set of plain R functions for clustering
samples from dominant markers, SNP genotypes (CSV, VCF or HapMap),
quantitative or mixed traits, and DNA or protein sequences. It computes
distance matrices, builds trees by UPGMA, WPGMA, single, complete or
Ward linkage, neighbour joining or BIONJ, and checks them with marker
bootstrap support (Felsenstein 1985
[doi:10.1111/j.1558-5646.1985.tb00420.x](https://doi.org/10.1111/j.1558-5646.1985.tb00420.x)
), cophenetic correlation, average silhouette width, the gap statistic
and Mantel tests. It also offers principal coordinates and principal
components, k-means and partitioning around medoids, and exports a
self-contained HTML report and an R script that reproduces the analysis.

## See also

Useful links:

- <https://github.com/ebenogoe/clusnalyst>

- <https://ebenogoe.github.io/clusnalyst/>

- Report bugs at <https://github.com/ebenogoe/clusnalyst/issues>

## Author

**Maintainer**: Ebenezer Ogoe <ebenezerogoe@gmail.com> \[copyright
holder\]

Other contributors:

- Michael Obeng (Co-author of the 2022 Clusnalyst app) \[contributor\]

- Stephen Amoako (Co-author of the 2022 Clusnalyst app) \[contributor\]
