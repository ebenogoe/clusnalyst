# Builds the simulated demo datasets in inst/extdata.
# Run from the package root: Rscript data-raw/make_demos.R
# Every dataset here is simulated. None of it comes from a real study.

set.seed(2022)
out <- "inst/extdata"

# 1. Dominant markers: 30 accessions from 3 groups, 60 bands -------------
groups <- rep(c("A", "B", "C"), each = 10)
ids <- sprintf("ACC%02d", 1:30)
base <- runif(60, 0.2, 0.8)
freq <- sapply(c("A", "B", "C"), function(g) pmin(pmax(base + rnorm(60, 0, 0.3), 0.03), 0.97))
bands <- t(sapply(groups, function(g) rbinom(60, 1, freq[, g])))
rownames(bands) <- ids
colnames(bands) <- sprintf("ISSR%02d_%d", rep(1:12, each = 5), rep(seq(250, 1250, 250), 12))
bands[sample(length(bands), round(0.02 * length(bands)))] <- NA
write.csv(data.frame(accession = ids, bands, check.names = FALSE), file.path(out, "demo_binary.csv"),
          row.names = FALSE, na = "")
write.csv(data.frame(accession = ids, group = paste("Group", groups)),
          file.path(out, "demo_binary_meta.csv"), row.names = FALSE)

# 2. SNPs: 60 lines from 3 populations, 1,000 biallelic SNPs --------------
# Balding-Nichols model with Fst = 0.12, written as letter genotypes with
# markers in rows (the layout most genotyping services deliver).
n_snp <- 1000
pops <- rep(c("North", "Middle", "South"), each = 20)
lines <- sprintf("L%03d", 1:60)
p_anc <- runif(n_snp, 0.05, 0.5)
fst <- 0.12
p_pop <- sapply(unique(pops), function(p) rbeta(n_snp, p_anc * (1 - fst) / fst, (1 - p_anc) * (1 - fst) / fst))
dos <- sapply(pops, function(p) rbinom(n_snp, 2, p_pop[, p]))
alleles <- t(replicate(n_snp, sample(c("A", "C", "G", "T"), 2)))
geno <- matrix(NA_character_, n_snp, 60)
for (j in 1:60) {
  geno[, j] <- ifelse(dos[, j] == 0, paste0(alleles[, 1], alleles[, 1]),
               ifelse(dos[, j] == 1, paste0(alleles[, 1], alleles[, 2]), paste0(alleles[, 2], alleles[, 2])))
}
geno[sample(length(geno), round(0.03 * length(geno)))] <- "NN"
colnames(geno) <- lines
snp_ids <- sprintf("S%d_%d", rep(1:10, each = 100), sort(sample(1e5:9e6, n_snp)))
write.csv(data.frame(snp = snp_ids, geno, check.names = FALSE), file.path(out, "demo_snp.csv"),
          row.names = FALSE)
centre <- list(North = c(10.5, -1.0), Middle = c(7.5, -1.5), South = c(5.8, -0.8))
coords <- t(sapply(pops, function(p) centre[[p]] + rnorm(2, 0, 0.5)))
write.csv(data.frame(line = lines, population = pops,
                     latitude = round(coords[, 1], 3), longitude = round(coords[, 2], 3)),
          file.path(out, "demo_snp_meta.csv"), row.names = FALSE)

# 3. Mixed traits: 40 cowpea-like accessions from 4 groups ----------------
k <- rep(1:4, each = 10)
acc <- sprintf("CP%02d", 1:40)
traits <- data.frame(
  accession = acc,
  plant_height_cm = round(rnorm(40, c(45, 60, 75, 52)[k], 6), 1),
  days_to_flowering = round(rnorm(40, c(42, 48, 55, 60)[k], 3)),
  seed_weight_g = round(rnorm(40, c(14, 18, 22, 16)[k], 1.5), 1),
  pod_length_cm = round(rnorm(40, c(14, 16, 19, 22)[k], 1.2), 1),
  seeds_per_pod = round(rnorm(40, c(12, 14, 15, 11)[k], 1.5)),
  seed_coat = c("cream", "brown", "black", "speckled")[ifelse(runif(40) < 0.8, k, sample(1:4, 40, TRUE))],
  growth_habit = c("erect", "semi-erect", "spreading", "semi-erect")[k]
)
traits$plant_height_cm[sample(40, 2)] <- NA
write.csv(traits, file.path(out, "demo_traits.csv"), row.names = FALSE, na = "")
write.csv(data.frame(accession = acc, group = paste("Group", k)),
          file.path(out, "demo_traits_meta.csv"), row.names = FALSE)
