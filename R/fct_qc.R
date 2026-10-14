#' Quality summary of a dataset
#'
#' For marker data: missing data per marker and per sample, minor allele
#' (or band) frequency, heterozygosity and monomorphic markers. For traits:
#' missing data and spread per trait. For sequences: lengths, gaps and
#' variable sites.
#'
#' @param d A `cl_data` object.
#' @return A list with `markers` and `samples` data frames and totals.
#' @export
qc_summary <- function(d) {
  switch(d$type,
    binary = , snp = qc_markers(d),
    quant = qc_traits(d),
    dna = , protein = qc_sequences(d)
  )
}

nan_to_na <- function(v) {
  v[is.nan(v)] <- NA
  v
}

qc_markers <- function(d) {
  x <- d$x
  miss <- colMeans(is.na(x))
  n_obs <- colSums(!is.na(x))
  if (d$type == "snp") {
    p <- nan_to_na(colMeans(x, na.rm = TRUE) / 2)
    het <- nan_to_na(colMeans(x == 1, na.rm = TRUE))
    s_het <- nan_to_na(rowMeans(x == 1, na.rm = TRUE))
  } else {
    p <- nan_to_na(colMeans(x, na.rm = TRUE))
    het <- rep(NA_real_, ncol(x))
    s_het <- rep(NA_real_, nrow(x))
  }
  maf <- pmin(p, 1 - p)
  mono <- is.na(maf) | maf == 0
  if (d$type == "snp") mono <- mono & (is.na(het) | het == 0)
  list(
    type = d$type,
    n_samples = nrow(x),
    n_markers = ncol(x),
    missing = mean(is.na(x)),
    n_monomorphic = sum(mono),
    markers = data.frame(marker = colnames(x), missing = miss, maf = maf,
                         heterozygosity = het, monomorphic = mono, n = n_obs,
                         row.names = NULL, check.names = FALSE),
    samples = data.frame(sample = rownames(x), missing = rowMeans(is.na(x)),
                         heterozygosity = s_het, row.names = NULL)
  )
}

qc_traits <- function(d) {
  x <- d$x
  num <- vapply(x, is.numeric, logical(1))
  tr <- data.frame(
    trait = names(x),
    kind = ifelse(num, "numeric", "categorical"),
    missing = vapply(x, function(v) mean(is.na(v)), numeric(1)),
    mean = vapply(x, function(v) if (is.numeric(v)) mean(v, na.rm = TRUE) else NA_real_, numeric(1)),
    sd = vapply(x, function(v) if (is.numeric(v)) stats::sd(v, na.rm = TRUE) else NA_real_, numeric(1)),
    min = vapply(x, function(v) if (is.numeric(v)) suppressWarnings(min(v, na.rm = TRUE)) else NA_real_, numeric(1)),
    max = vapply(x, function(v) if (is.numeric(v)) suppressWarnings(max(v, na.rm = TRUE)) else NA_real_, numeric(1)),
    levels = vapply(x, function(v) length(unique(stats::na.omit(v))), numeric(1)),
    row.names = NULL
  )
  tr$constant <- tr$levels <= 1
  list(
    type = "quant",
    n_samples = nrow(x),
    n_markers = ncol(x),
    missing = mean(is.na(as.matrix(x))),
    n_monomorphic = sum(tr$constant),
    n_categorical = sum(!num),
    markers = tr,
    samples = data.frame(sample = rownames(x), missing = rowMeans(is.na(x)), row.names = NULL)
  )
}

qc_sequences <- function(d) {
  len <- nchar(gsub("-", "", d$seqs, fixed = TRUE))
  out <- list(
    type = d$type,
    n_samples = length(d$seqs),
    aligned = isTRUE(d$aligned),
    lengths = len,
    samples = data.frame(sample = names(d$seqs), length = len, row.names = NULL)
  )
  if (!is.null(d$x)) {
    gap <- d$x %in% c("-", "?", "N", "X")
    dim(gap) <- dim(d$x)
    var_site <- apply(d$x, 2, function(s) length(unique(s[!s %in% c("-", "?", "N", "X")])) > 1)
    out$n_markers <- ncol(d$x)
    out$n_variable <- sum(var_site)
    out$missing <- mean(gap)
    out$samples$gaps <- rowMeans(gap)
    out$samples$missing <- rowMeans(gap)
  } else {
    out$n_markers <- NA_integer_
    out$missing <- NA_real_
  }
  out
}

#' Filter markers, traits or sequences
#'
#' Markers (or traits) are filtered first, then samples are checked against
#' the markers that are left.
#'
#' @param d A `cl_data` object.
#' @param max_marker_missing Largest share of missing values allowed per
#'   marker or trait (0 to 1).
#' @param min_maf Smallest minor allele (or band) frequency kept (marker
#'   data only).
#' @param drop_monomorphic Drop markers or traits that do not vary.
#' @param max_sample_missing Largest share of missing values allowed per
#'   sample.
#' @param drop_empty_sites For aligned sequences, drop sites that are gaps
#'   in every sequence.
#' @return The filtered `cl_data`, with a `filter` element recording what
#'   was removed.
#' @export
filter_data <- function(d, max_marker_missing = 0.2, min_maf = 0,
                        drop_monomorphic = TRUE, max_sample_missing = 0.5,
                        drop_empty_sites = TRUE) {
  before <- c(samples = n_samples(d), columns = n_columns(d))
  settings <- list(max_marker_missing = max_marker_missing, min_maf = min_maf,
                   drop_monomorphic = drop_monomorphic,
                   max_sample_missing = max_sample_missing)
  if (d$type %in% c("binary", "snp")) {
    q <- qc_markers(d)$markers
    keep <- q$missing <= max_marker_missing & (is.na(q$maf) | q$maf >= min_maf)
    if (drop_monomorphic) keep <- keep & !q$monomorphic
    keep[is.na(keep)] <- FALSE
    d$x <- d$x[, keep, drop = FALSE]
    s_keep <- rowMeans(is.na(d$x)) <= max_sample_missing
    removed <- rownames(d$x)[!s_keep]
    d$x <- d$x[s_keep, , drop = FALSE]
  } else if (d$type == "quant") {
    q <- qc_traits(d)$markers
    keep <- q$missing <= max_marker_missing
    if (drop_monomorphic) keep <- keep & !q$constant
    d$x <- d$x[, keep, drop = FALSE]
    s_keep <- rowMeans(is.na(d$x)) <= max_sample_missing
    removed <- rownames(d$x)[!s_keep]
    d$x <- d$x[s_keep, , drop = FALSE]
  } else {
    if (is.null(d$x)) cl_abort("Align the sequences before filtering.")
    gap <- d$x %in% c("-", "?", "N", "X")
    dim(gap) <- dim(d$x)
    s_keep <- rowMeans(gap) <= max_sample_missing
    removed <- rownames(d$x)[!s_keep]
    d$x <- d$x[s_keep, , drop = FALSE]
    d$seqs <- d$seqs[s_keep]
    if (drop_empty_sites) {
      gap <- gap[s_keep, , drop = FALSE]
      d$x <- d$x[, colSums(!gap) > 0, drop = FALSE]
    }
    settings <- list(max_sample_missing = max_sample_missing,
                     drop_empty_sites = drop_empty_sites)
  }
  if (!is.null(d$meta)) d$meta <- d$meta[rownames(d$x), , drop = FALSE]
  if (ncol(d$x) < 1) cl_abort("No ", unit_label(d$type), " are left after filtering. Loosen the thresholds.")
  if (nrow(d$x) < 3) cl_abort("Fewer than three samples are left after filtering. Loosen the thresholds.")
  d$filter <- list(
    settings = settings,
    before = before,
    after = c(samples = nrow(d$x), columns = ncol(d$x)),
    removed_samples = removed
  )
  d
}
