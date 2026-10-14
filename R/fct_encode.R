#' Convert letter genotypes to allele dosages
#'
#' Accepts two-letter calls (`AA`, `AG`), separated calls (`A/G`, `A|G`) and
#' single IUPAC codes (`A`, `R`, `Y`, ...). For each marker the most common
#' allele is the reference, so a dosage counts copies of the other allele:
#' 0 = homozygous for the common allele, 1 = heterozygous, 2 = homozygous
#' for the other allele. Markers with more than two alleles count every
#' allele that is not the common one.
#'
#' @param m Character matrix with samples in rows and markers in columns.
#' @return Numeric matrix of 0, 1, 2 and `NA`, same dimensions as `m`.
#' @export
#' @examples
#' genotypes_to_dosage(matrix(c("AA", "AG", "GG", "R", "NN", "A/G"), 3))
genotypes_to_dosage <- function(m) {
  m <- toupper(trimws(as.matrix(m)))
  dn <- dimnames(m)
  missing <- c("", "NA", "N", "NN", "--", "-", "0", "00", "./.", ".|.", ".", "??", "?")
  m[m %in% missing] <- NA
  m <- gsub("[/|:]", "", m)
  iupac <- c(R = "AG", Y = "CT", S = "CG", W = "AT", K = "GT", M = "AC",
             A = "AA", C = "CC", G = "GG", T = "TT")
  one <- !is.na(m) & nchar(m) == 1
  m[one] <- unname(iupac[m[one]])
  m[!is.na(m) & !grepl("^[ACGT]{2}$", m)] <- NA
  a1 <- substr(m, 1, 1)
  a2 <- substr(m, 2, 2)
  bases <- c("A", "C", "G", "T")
  counts <- vapply(bases, function(b) {
    colSums(a1 == b, na.rm = TRUE) + colSums(a2 == b, na.rm = TRUE)
  }, numeric(ncol(m)))
  counts <- matrix(counts, ncol = 4)
  major <- bases[max.col(counts, ties.method = "first")]
  maj <- matrix(major, nrow(m), ncol(m), byrow = TRUE)
  out <- (a1 != maj) + (a2 != maj)
  storage.mode(out) <- "double"
  dimnames(out) <- dn
  out
}
