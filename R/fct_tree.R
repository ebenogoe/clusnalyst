#' Tree-building methods
#'
#' @return A named character vector (labels as names).
#' @export
tree_choices <- function() {
  c(
    "UPGMA (average linkage)" = "upgma",
    "WPGMA (McQuitty)" = "wpgma",
    "Single linkage" = "single",
    "Complete linkage" = "complete",
    "Ward (Ward.D2)" = "ward",
    "Neighbour joining (NJ)" = "nj",
    "BIONJ" = "bionj"
  )
}

hclust_methods <- c(upgma = "average", wpgma = "mcquitty", single = "single",
                    complete = "complete", ward = "ward.D2")

tree_label <- function(method) names(tree_choices())[match(method, tree_choices())]

is_hierarchical <- function(method) method %in% names(hclust_methods)

#' Build a tree from a distance matrix
#'
#' Hierarchical methods give a rooted, ultrametric tree. NJ and BIONJ give
#' an unrooted tree; give `outgroup` to root it. Negative branch lengths
#' from NJ are set to zero.
#'
#' @param dist A `dist` object.
#' @param method A method from [tree_choices()].
#' @param outgroup Optional sample name(s) to root an NJ or BIONJ tree on.
#' @return A `cl_tree` list with `phylo` (an `ape` tree), `hclust` (for
#'   hierarchical methods), `method` and `rooted`.
#' @export
#' @examples
#' d <- clusnalyst_demo("binary")
#' tr <- build_tree(compute_distance(d), "upgma")
build_tree <- function(dist, method = "upgma", outgroup = NULL) {
  if (!method %in% tree_choices()) cl_abort("Unknown tree method: ", method)
  if (attr(dist, "Size") < 3) cl_abort("At least three samples are needed to build a tree.")
  hc <- NULL
  if (is_hierarchical(method)) {
    hc <- stats::hclust(dist, method = hclust_methods[[method]])
    phy <- ape::as.phylo(hc)
    rooted <- TRUE
  } else {
    phy <- if (method == "nj") ape::nj(dist) else ape::bionj(dist)
    phy$edge.length[phy$edge.length < 0] <- 0
    rooted <- FALSE
    outgroup <- intersect(outgroup, phy$tip.label)
    if (length(outgroup)) {
      phy <- ape::root(phy, outgroup = outgroup, resolve.root = TRUE)
      rooted <- TRUE
    }
  }
  structure(
    list(phylo = phy, hclust = hc, method = method, rooted = rooted,
         outgroup = if (length(outgroup)) outgroup else NULL),
    class = "cl_tree"
  )
}

#' @export
print.cl_tree <- function(x, ...) {
  cat("<Clusnalyst tree>", tree_label(x$method), "with", ape::Ntip(x$phylo), "tips\n")
  invisible(x)
}

tree_cophenetic <- function(tree, labels) {
  if (!is.null(tree$hclust)) {
    m <- as.matrix(stats::cophenetic(tree$hclust))
  } else {
    m <- ape::cophenetic.phylo(tree$phylo)
  }
  stats::as.dist(m[labels, labels])
}

#' Cophenetic correlation of a tree
#'
#' The correlation between the distances you started with and the
#' distances implied by the tree. Values near 1 mean the tree keeps the
#' original distances well.
#'
#' @param dist A `dist` object.
#' @param tree A `cl_tree`.
#' @return A number.
#' @export
cophenetic_cor <- function(dist, tree) {
  stats::cor(as.vector(dist), as.vector(tree_cophenetic(tree, labels(dist))))
}

#' Compare tree methods by cophenetic correlation
#'
#' @param dist A `dist` object.
#' @param methods Methods to compare.
#' @return A data frame sorted from the best-fitting method down.
#' @export
cophenetic_table <- function(dist, methods = tree_choices()) {
  r <- vapply(methods, function(m) {
    tryCatch(cophenetic_cor(dist, build_tree(dist, m)), error = function(e) NA_real_)
  }, numeric(1))
  out <- data.frame(method = unname(methods), label = tree_label(methods),
                    cophenetic_r = unname(r))
  out <- out[order(-out$cophenetic_r), ]
  rownames(out) <- NULL
  out
}
