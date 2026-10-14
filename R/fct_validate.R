#' Bootstrap support for a tree
#'
#' Resamples the markers, traits or alignment sites with replacement,
#' recomputes the distance matrix and the tree for each replicate, and
#' reports how often each group in the original tree is found again
#' (Felsenstein 1985). For rooted trees the groups are clades; for unrooted
#' NJ and BIONJ trees they are bipartitions.
#'
#' @param d The `cl_data` the tree was built from.
#' @param tree A `cl_tree` from [build_tree()].
#' @param distance Distance method used for the tree.
#' @param B Number of bootstrap replicates.
#' @param seed Random seed, so the run can be repeated exactly.
#' @param sqrt_binary,scale Passed to [compute_distance()].
#' @param progress Optional function called with the share done (0 to 1).
#' @return A list with `support` (percent, one value per internal node of
#'   `tree$phylo`), `trees` (the replicate trees), `B`, `failed` and
#'   `seed`.
#' @export
bootstrap_tree <- function(d, tree, distance = NULL, B = 100, seed = 1,
                           sqrt_binary = FALSE, scale = TRUE, progress = NULL) {
  distance <- distance %||% default_distance(d)
  n_col <- ncol(d$x)
  if (n_col < 2) cl_abort("Bootstrapping needs at least two ", unit_label(d$type), ".")
  reps <- vector("list", B)
  failed <- 0L
  idx_all <- with_seed(seed, lapply(seq_len(B), function(b) sample.int(n_col, n_col, replace = TRUE)))
  for (b in seq_len(B)) {
    db <- d
    db$x <- d$x[, idx_all[[b]], drop = FALSE]
    if (is.data.frame(db$x)) names(db$x) <- make.unique(names(db$x))
    reps[[b]] <- tryCatch({
      dm <- compute_distance(db, distance, sqrt_binary = sqrt_binary, scale = scale)
      build_tree(dm, tree$method, outgroup = tree$outgroup)$phylo
    }, error = function(e) NULL)
    if (is.null(reps[[b]])) failed <- failed + 1L
    if (!is.null(progress)) progress(b / B)
  }
  reps <- Filter(Negate(is.null), reps)
  if (!length(reps)) cl_abort("Every bootstrap replicate failed. Check for missing data.")
  class(reps) <- "multiPhylo"
  counts <- ape::prop.clades(tree$phylo, reps, rooted = tree$rooted)
  counts[is.na(counts)] <- 0
  list(
    support = 100 * counts / length(reps),
    trees = reps,
    B = length(reps),
    failed = failed,
    seed = seed,
    rooted = tree$rooted
  )
}

#' Majority-rule consensus of bootstrap trees
#' @param boot Result of [bootstrap_tree()].
#' @param p Share of trees a group must appear in (0.5 = majority rule).
#' @return An `ape` tree.
#' @export
consensus_tree <- function(boot, p = 0.5) {
  ape::consensus(boot$trees, p = p, rooted = boot$rooted)
}

#' How to read an average silhouette width
#'
#' Bands from Kaufman and Rousseeuw (1990).
#' @param asw Average silhouette width.
#' @return A short phrase.
#' @export
silhouette_meaning <- function(asw) {
  ifelse(asw > 0.70, "a strong structure",
    ifelse(asw > 0.50, "a reasonable structure",
      ifelse(asw > 0.25, "a weak structure that could be artificial", "no substantial structure")))
}

#' Choose the number of clusters
#'
#' For each k from 2 to `k_max`, splits the samples into k groups and
#' records the average silhouette width (Rousseeuw 1987), the within-group
#' sum of squares (for an elbow plot) and, optionally, the gap statistic
#' (Tibshirani et al. 2001). Hierarchical trees are cut with
#' [stats::cutree()]; NJ and BIONJ trees cannot be cut that way, so
#' partitioning around medoids (PAM) is used instead.
#'
#' @param dist A `dist` object.
#' @param tree Optional `cl_tree`. If it is hierarchical, its cuts are used.
#' @param k_max Largest k to try.
#' @param gap Also compute the gap statistic (slower).
#' @param gap_B Reference datasets for the gap statistic.
#' @param seed Random seed for the gap statistic.
#' @return A list with a `table` (one row per k), `best_silhouette`,
#'   `best_gap` and `clustering` (how the groups were formed).
#' @export
optimal_k <- function(dist, tree = NULL, k_max = 10, gap = FALSE, gap_B = 50, seed = 1) {
  n <- attr(dist, "Size")
  k_max <- min(k_max, n - 1)
  if (k_max < 2) cl_abort("At least three samples are needed to compare cluster numbers.")
  use_tree <- !is.null(tree) && !is.null(tree$hclust)
  Dm <- as.matrix(dist)
  ks <- 2:k_max
  rows <- lapply(ks, function(k) {
    cl <- if (use_tree) stats::cutree(tree$hclust, k) else cluster::pam(dist, k, diss = TRUE, cluster.only = TRUE)
    sil <- if (length(unique(cl)) > 1) mean(cluster::silhouette(cl, dist)[, "sil_width"]) else NA_real_
    wss <- sum(vapply(split(seq_len(n), cl), function(i) sum(Dm[i, i]^2) / (2 * length(i)), numeric(1)))
    c(k = k, silhouette = sil, wss = wss)
  })
  tab <- as.data.frame(do.call(rbind, rows))
  tab$gap <- NA_real_
  tab$gap_se <- NA_real_
  best_gap <- NA_integer_
  if (gap) {
    coords <- stats::cmdscale(dist, k = min(n - 1, 10))
    g <- with_seed(seed, cluster::clusGap(
      coords, FUNcluster = function(x, k) list(cluster = cluster::pam(x, k, cluster.only = TRUE)),
      K.max = k_max, B = gap_B, verbose = FALSE
    ))$Tab
    tab$gap <- g[ks, "gap"]
    tab$gap_se <- g[ks, "SE.sim"]
    best_gap <- cluster::maxSE(g[, "gap"], g[, "SE.sim"], method = "firstSEmax")
  }
  list(
    table = tab,
    best_silhouette = tab$k[which.max(tab$silhouette)],
    best_gap = best_gap,
    clustering = if (use_tree) "tree cut" else "PAM"
  )
}

#' Mantel test between two distance matrices
#'
#' Uses the samples the two matrices share.
#'
#' @param d1,d2 `dist` objects (or square matrices) with sample names.
#' @param permutations Number of permutations.
#' @param seed Random seed.
#' @return A list with `r`, `p`, `n` and `permutations`.
#' @export
mantel_test <- function(d1, d2, permutations = 999, seed = 1) {
  m1 <- as.matrix(d1)
  m2 <- as.matrix(d2)
  ids <- intersect(rownames(m1), rownames(m2))
  if (length(ids) < 4) cl_abort("The two matrices share fewer than four samples.")
  a <- stats::as.dist(m1[ids, ids])
  b <- stats::as.dist(m2[ids, ids])
  res <- with_seed(seed, ade4::mantel.rtest(a, b, nrepet = permutations))
  list(r = unname(res$obs), p = unname(res$pvalue), n = length(ids),
       permutations = permutations,
       pairs = data.frame(d1 = as.vector(a), d2 = as.vector(b)))
}

#' Read a second matrix for a Mantel test
#'
#' A square table whose row and column names are the same samples is read
#' as a distance matrix. Anything else is read as data with
#' [read_markers()] and turned into distances with the default measure for
#' its type.
#'
#' @param path Path to the file.
#' @param name Original file name.
#' @return A `dist` object.
#' @export
read_second_matrix <- function(path, name = basename(path)) {
  fmt <- detect_format(path, name)
  if (fmt %in% c("csv", "csv2", "tsv", "excel")) {
    tab <- read_table_file(path, fmt)
    if (nrow(tab) == ncol(tab) - 1 && setequal(tab[[1]], names(tab)[-1]) && all(vapply(tab[-1], is_numberish, logical(1)))) {
      m <- as.matrix(as.data.frame(lapply(tab[-1], as.numeric)))
      dimnames(m) <- list(tab[[1]], names(tab)[-1])
      m <- m[tab[[1]], tab[[1]]]
      out <- stats::as.dist(m)
      attr(out, "label") <- "uploaded distance matrix"
      return(out)
    }
  }
  d <- read_markers(path, name)
  compute_distance(d)
}

#' Distances from numeric metadata columns
#'
#' Useful for a Mantel test of genetic distance against geography (for
#' example latitude and longitude) or any measured covariates.
#'
#' @param meta A data frame with sample names as row names.
#' @param columns Numeric columns to use.
#' @param scale Scale the columns first.
#' @return A Euclidean `dist` object.
#' @export
metadata_distance <- function(meta, columns, scale = FALSE) {
  m <- as.matrix(meta[, columns, drop = FALSE])
  if (!is.numeric(m)) cl_abort("Choose numeric metadata columns.")
  m <- m[stats::complete.cases(m), , drop = FALSE]
  if (scale) m <- base::scale(m)
  out <- stats::dist(m)
  attr(out, "label") <- paste("Euclidean distance on", paste(columns, collapse = ", "))
  out
}

# Run code with a fixed seed and put the caller's random number state back
with_seed <- function(seed, code) {
  if (is.null(seed)) return(code)
  env <- globalenv()
  had <- exists(".Random.seed", envir = env, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = env, inherits = FALSE)
  on.exit(if (had) assign(".Random.seed", old, envir = env) else rm(".Random.seed", envir = env))
  set.seed(seed)
  code
}
