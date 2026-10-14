numeric_matrix <- function(d) {
  if (d$type %in% c("binary", "snp")) {
    m <- d$x
  } else if (d$type == "quant") {
    num <- vapply(d$x, is.numeric, logical(1))
    if (!any(num)) return(NULL)
    m <- as.matrix(d$x[, num, drop = FALSE])
  } else {
    return(NULL)
  }
  # Mean-impute missing values and drop columns that do not vary
  for (j in seq_len(ncol(m))) {
    miss <- is.na(m[, j])
    if (any(miss)) m[miss, j] <- mean(m[, j], na.rm = TRUE)
  }
  sds <- apply(m, 2, stats::sd)
  m[, is.finite(sds) & sds > 0, drop = FALSE]
}

#' Can PCA or k-means be used on this dataset?
#' @param d A `cl_data` object.
#' @return `TRUE` for marker data and for data with numeric traits.
#' @export
has_numeric <- function(d) {
  d$type %in% c("binary", "snp") ||
    (d$type == "quant" && any(vapply(d$x, is.numeric, logical(1))))
}

#' Principal coordinates or principal components
#'
#' PCoA (classical multidimensional scaling) works from any distance
#' matrix. PCA needs numeric data; missing values are replaced by the
#' column mean, and traits (not markers) are scaled to unit variance.
#'
#' @param dist A `dist` object (for PCoA).
#' @param d A `cl_data` object (for PCA).
#' @param method `"pcoa"` or `"pca"`.
#' @param n_axes Number of axes to keep.
#' @return A `cl_ord` list with `scores`, `variance` (percent for every
#'   axis) and `method`.
#' @export
ordinate <- function(dist = NULL, d = NULL, method = c("pcoa", "pca"), n_axes = 5) {
  method <- match.arg(method)
  if (method == "pcoa") {
    n <- attr(dist, "Size")
    e <- stats::cmdscale(dist, k = min(n - 1, n_axes), eig = TRUE)
    pos <- e$eig[e$eig > 1e-10]
    var <- 100 * pos / sum(pos)
    scores <- e$points
    neg <- sum(abs(e$eig[e$eig < -1e-10])) / sum(abs(e$eig))
  } else {
    m <- numeric_matrix(d)
    if (is.null(m) || ncol(m) < 2) cl_abort("PCA needs at least two numeric markers or traits that vary.")
    p <- stats::prcomp(m, center = TRUE, scale. = d$type == "quant")
    var <- 100 * p$sdev^2 / sum(p$sdev^2)
    scores <- p$x[, seq_len(min(n_axes, ncol(p$x))), drop = FALSE]
    neg <- 0
  }
  colnames(scores) <- paste0("Axis", seq_len(ncol(scores)))
  structure(
    list(scores = as.data.frame(scores), variance = var, method = method,
         negative_eigen = neg),
    class = "cl_ord"
  )
}

#' Split samples into groups
#'
#' @param dist A `dist` object.
#' @param k Number of groups.
#' @param method `"tree"` (cut a hierarchical tree), `"pam"` (partitioning
#'   around medoids, any distance) or `"kmeans"` (numeric data only).
#' @param tree A hierarchical `cl_tree`, for `method = "tree"`.
#' @param d A `cl_data` object, for `method = "kmeans"`.
#' @param seed Random seed for k-means.
#' @return A named integer vector of group numbers. Groups are numbered in
#'   the order they first appear down the tree when a tree is given.
#' @export
partition <- function(dist, k, method = c("tree", "pam", "kmeans"), tree = NULL,
                      d = NULL, seed = 1) {
  method <- match.arg(method)
  n <- attr(dist, "Size")
  if (k < 2 || k >= n) cl_abort("k must be between 2 and ", n - 1, ".")
  cl <- switch(method,
    tree = {
      if (is.null(tree$hclust)) cl_abort("Only hierarchical trees can be cut into groups. Use PAM for NJ trees.")
      stats::cutree(tree$hclust, k)
    },
    pam = cluster::pam(dist, k, diss = TRUE, cluster.only = TRUE),
    kmeans = {
      m <- numeric_matrix(d)
      if (is.null(m)) cl_abort("k-means needs numeric data.")
      if (d$type == "quant") m <- base::scale(m)
      with_seed(seed, stats::kmeans(m, centers = k, nstart = 25))$cluster
    }
  )
  if (is.null(names(cl))) names(cl) <- labels(dist)
  ord <- if (!is.null(tree)) tip_order(tree$phylo) else labels(dist)
  relabel_groups(cl, ord)
}

tip_order <- function(phy) {
  is_tip <- phy$edge[, 2] <= ape::Ntip(phy)
  phy$tip.label[phy$edge[is_tip, 2]]
}

relabel_groups <- function(cl, order) {
  order <- intersect(order, names(cl))
  first <- unique(cl[order])
  out <- match(cl, first)
  names(out) <- names(cl)
  out
}
