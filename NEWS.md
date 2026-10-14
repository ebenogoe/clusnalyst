# clusnalyst 0.1.0

First release of the rewritten Clusnalyst, as an R package with the app inside.

## Compared with the 2022 app

* Reads 0/1 band tables, SNP genotypes as letters, 0/1/2, VCF or HapMap, measured and categorical traits, and DNA or protein FASTA. The data type and table layout are detected and can be overridden.
* Quality check step with filters for missing data, minor allele frequency and markers that do not vary. Sequences are aligned with ClustalOmega, ClustalW or MUSCLE.
* New distances: allele sharing (IBS), modified Rogers, Euclidean, Manhattan, Gower, and K80, JC69, TN93 and p-distance for sequences.
* New tree methods: WPGMA, Ward, BIONJ, and NJ rooted on an outgroup. A cophenetic correlation table compares them.
* Number of groups from silhouette width, the within-group sum of squares and the gap statistic. Mantel tests against a second matrix or against numeric sample information such as location.
* PCoA and PCA, and groups from cutting the tree, PAM or k-means.
* Interactive trees and plots, sample information on hover, light and dark themes, a guided tour.
* Downloads: PNG, SVG and PDF for every figure, CSV and Excel tables, Newick trees, an HTML report, an R script that repeats the analysis, and a session file.

## Fixed from the 2022 app

* The "Single" and "Complete" options ran Ward.D and Ward.D2.
* Bootstrap support came from resampling the columns of the distance matrix. It now resamples markers (or alignment sites) and rebuilds the distances, as in Felsenstein (1985).
* Data were kept in global variables, so people using the hosted app at the same time could overwrite each other's results. Each session now has its own state.
* Choosing "already aligned" for a FASTA file crashed, and DNA could not be read.
* Removing monomorphic markers failed when there were none, and letter genotypes with missing calls were coded wrongly.
* Silhouette analysis failed for neighbour-joining trees.
