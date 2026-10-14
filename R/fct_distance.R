#' Distance measures available for a data type
#'
#' @param type A data type from [data_types()].
#' @return A named character vector (labels as names). The first entry is
#'   the default.
#' @export
distance_choices <- function(type) {
  switch(type,
    binary = c("Jaccard" = "jaccard", "Dice (Sorensen)" = "dice",
               "Simple matching" = "simple_matching"),
    snp = c("Allele sharing (IBS)" = "ibs", "Modified Rogers" = "rogers",
            "Euclidean" = "euclidean"),
    quant = c("Euclidean" = "euclidean", "Manhattan" = "manhattan", "Gower" = "gower"),
    dna = c("Kimura 2-parameter (K80)" = "K80", "Proportion of differing sites (p)" = "raw",
            "Jukes and Cantor (JC69)" = "JC69", "Tamura and Nei (TN93)" = "TN93"),
    protein = c("Proportion of differing sites (p)" = "p",
                "Identity (seqinr)" = "identity", "Similarity (seqinr)" = "similarity"),
    cl_abort("Unknown data type: ", type)
  )
}

#' Default distance for a dataset
#' @param d A `cl_data` object.
#' @return A method name.
#' @export
default_distance <- function(d) {
  if (d$type == "quant" && !all(vapply(d$x, is.numeric, logical(1)))) return("gower")
  unname(distance_choices(d$type)[1])
}

distance_label <- function(type, method) {
  ch <- distance_choices(type)
  names(ch)[match(method, ch)]
}

#' Compute a distance matrix
#'
#' Missing values are handled pairwise: each pair of samples is compared on
#' the markers (or sites) both of them have.
#'
#' Binary measures are returned as 1 minus the similarity. Set
#' `sqrt_binary = TRUE` for the square root form that `ade4::dist.binary()`
#' returns, which is Euclidean. For biallelic SNPs, IBS distance equals
#' Prevosti's distance.
#'
#' @param d A `cl_data` object.
#' @param method A method from [distance_choices()]; the default for the
#'   data type if `NULL`.
#' @param sqrt_binary Use sqrt(1 - S) for binary measures.
#' @param scale Scale numeric traits to unit variance before Euclidean or
#'   Manhattan distances.
#' @return A `dist` object with a `method` attribute.
#' @export
#' @examples
#' d <- clusnalyst_demo("binary")
#' dm <- compute_distance(d, "jaccard")
compute_distance <- function(d, method = NULL, sqrt_binary = FALSE, scale = TRUE) {
  method <- method %||% default_distance(d)
  if (!method %in% distance_choices(d$type)) {
    cl_abort("'", method, "' is not a distance for ", tolower(type_label(d$type)), ".")
  }
  if (is.null(d$x)) cl_abort("Align the sequences before computing distances.")
  D <- switch(d$type,
    binary = binary_distance(d$x, method, sqrt_binary),
    snp = snp_distance(d$x, method),
    quant = trait_distance(d$x, method, scale),
    dna = dna_distance(d$x, method),
    protein = protein_distance(d$x, method)
  )
  D <- as.matrix(D)
  if (anyNA(D) || any(!is.finite(D))) {
    bad <- which(!is.finite(D), arr.ind = TRUE)[1, ]
    cl_abort("The distance between ", rownames(D)[bad[1]], " and ", colnames(D)[bad[2]],
             " could not be computed. They may share no scored ",
             unit_label(d$type), ", or (for sequence models) be too divergent. ",
             "Try filtering missing data or a simpler measure.")
  }
  diag(D) <- 0
  out <- stats::as.dist(D)
  attr(out, "method") <- method
  attr(out, "label") <- distance_label(d$type, method)
  if (d$type == "binary" && sqrt_binary) attr(out, "label") <- paste0(attr(out, "label"), ", sqrt(1 - S)")
  out
}

binary_distance <- function(x, method, sqrt_form = FALSE) {
  ok <- !is.na(x)
  X1 <- (x == 1 & ok) * 1
  X0 <- (x == 0 & ok) * 1
  X1[is.na(X1)] <- 0
  X0[is.na(X0)] <- 0
  a <- tcrossprod(X1)
  dd <- tcrossprod(X0)
  b <- tcrossprod(X1, X0)
  c <- t(b)
  s <- switch(method,
    jaccard = a / (a + b + c),
    dice = 2 * a / (2 * a + b + c),
    simple_matching = (a + dd) / (a + b + c + dd)
  )
  # Two samples with no bands at all on their shared markers are identical
  if (method != "simple_matching") s[(a + b + c) == 0 & (a + b + c + dd) > 0] <- 1
  D <- 1 - s
  if (sqrt_form) D <- sqrt(D)
  D
}

snp_distance <- function(x, method) {
  if (method == "euclidean") return(as.matrix(stats::dist(x)))
  ind <- lapply(0:2, function(v) {
    m <- (x == v) * 1
    m[is.na(m)] <- 0
    m
  })
  cc <- function(u, v) tcrossprod(ind[[u + 1]], ind[[v + 1]])
  d1 <- cc(0, 1) + cc(1, 0) + cc(1, 2) + cc(2, 1)
  d2 <- cc(0, 2) + cc(2, 0)
  ok <- (!is.na(x)) * 1
  n <- tcrossprod(ok)
  D <- switch(method,
    ibs = (d1 + 2 * d2) / (2 * n),
    rogers = sqrt((0.25 * d1 + d2) / n)
  )
  dimnames(D) <- list(rownames(x), rownames(x))
  D
}

trait_distance <- function(x, method, scale = TRUE) {
  num <- vapply(x, is.numeric, logical(1))
  if (method == "gower") {
    df <- x
    df[!num] <- lapply(df[!num], factor)
    return(as.matrix(cluster::daisy(df, metric = "gower")))
  }
  if (!all(num)) {
    cl_abort("Euclidean and Manhattan distances need numeric traits only. ",
             "Your data has categorical traits (", paste(names(x)[!num], collapse = ", "),
             "), so use Gower.")
  }
  m <- as.matrix(x)
  if (scale) {
    sds <- apply(m, 2, stats::sd, na.rm = TRUE)
    m <- m[, is.finite(sds) & sds > 0, drop = FALSE]
    m <- base::scale(m)
  }
  as.matrix(stats::dist(m, method = method))
}

dna_distance <- function(x, method) {
  bin <- ape::as.DNAbin(tolower(x))
  as.matrix(ape::dist.dna(bin, model = method, pairwise.deletion = TRUE))
}

protein_distance <- function(x, method) {
  if (method == "p") return(p_distance(x))
  seqs <- apply(x, 1, paste, collapse = "")
  aln <- seqinr::as.alignment(nb = length(seqs), nam = names(seqs),
                              seq = tolower(unname(seqs)), com = NA)
  D <- as.matrix(seqinr::dist.alignment(aln, matrix = method))
  dimnames(D) <- list(names(seqs), names(seqs))
  D
}

p_distance <- function(x, gaps = c("-", "?", "X", "N")) {
  n <- nrow(x)
  L <- ncol(x)
  ok <- !(x %in% gaps)
  dim(ok) <- dim(x)
  D <- matrix(0, n, n, dimnames = list(rownames(x), rownames(x)))
  for (i in seq_len(n)) {
    xi <- matrix(x[i, ], n, L, byrow = TRUE)
    oki <- matrix(ok[i, ], n, L, byrow = TRUE)
    valid <- ok & oki
    D[i, ] <- rowSums((x != xi) & valid) / rowSums(valid)
  }
  D
}
