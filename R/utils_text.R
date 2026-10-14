# Plain-language notes on each method, shown next to the choices

distance_notes <- list(
  jaccard = "Bands shared by both samples, divided by bands present in either. Shared absences are ignored, which suits dominant markers: two samples both lacking a band may lack it for different reasons (Jaccard 1901).",
  dice = "Like Jaccard, but shared bands count twice, so it puts more weight on what samples have in common (Dice 1945; Nei and Li 1979).",
  simple_matching = "Counts shared absences as well as shared bands. Use it when a 0 tells you as much as a 1 (Sokal and Michener 1958).",
  ibs = "The share of alleles that differ between two samples, averaged over SNPs: 0 means identical, 1 means no alleles in common. For biallelic SNPs this equals Prevosti's distance.",
  rogers = "Modified Rogers distance (Wright 1978; Goodman and Stuber 1983), a Euclidean distance on allele frequencies. Widely used in maize and sorghum diversity work.",
  euclidean = "Straight-line distance between the samples' profiles. For traits, scale first unless every trait shares the same unit.",
  manhattan = "Sum of absolute differences. A single very different trait pulls it less than it pulls Euclidean distance.",
  gower = "Treats numeric and categorical traits together, each counting equally (Gower 1971). The only choice here when some traits are categories.",
  K80 = "Kimura's two-parameter model (1980). It corrects for repeated changes at the same site and lets transitions and transversions happen at different rates.",
  raw = "The share of aligned sites that differ. No correction for repeated changes, so it underestimates large distances.",
  JC69 = "Jukes and Cantor (1969). Corrects for repeated changes, assuming every substitution is equally likely.",
  TN93 = "Tamura and Nei (1993). Allows unequal base frequencies and two kinds of transitions. Needs more sites to estimate well.",
  p = "The share of aligned positions that differ, skipping gaps.",
  identity = "seqinr's identity distance: the square root of (1 minus the share of identical positions).",
  similarity = "seqinr's similarity distance, which counts chemically similar amino acids as matches."
)

tree_notes <- list(
  upgma = "Joins the closest pair, then treats it as one sample whose distance to the rest is the average. Gives a rooted tree with all tips the same distance from the root. The most used method for diversity studies.",
  wpgma = "Like UPGMA, but each of the two joined groups counts equally, whatever their size.",
  single = "Groups are as close as their two closest members. Tends to make long chains, so it is rarely the best fit.",
  complete = "Groups are as close as their two most distant members. Gives compact groups of similar size.",
  ward = "Joins the pair of groups that adds least to the within-group spread (Ward.D2, which squares the distances first). Gives compact, even groups.",
  nj = "Neighbour joining (Saitou and Nei 1987) does not assume every lineage changes at the same rate, so branch lengths can differ. The tree is unrooted unless you give an outgroup.",
  bionj = "An improved neighbour joining (Gascuel 1997) that weights the distances by how reliable they are. Often a little more accurate than NJ."
)
