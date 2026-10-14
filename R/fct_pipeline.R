#' Default analysis settings
#'
#' Every step of the app is described by one entry in this list. The app
#' fills it in as you go, [run_analysis()] replays it, and
#' [build_script()] turns it into R code.
#'
#' @param d Optional `cl_data`; picks defaults that suit its type.
#' @return A named list.
#' @export
default_settings <- function(d = NULL) {
  type <- if (is.null(d)) "binary" else d$type
  tree_method <- if (type %in% c("dna", "protein")) "nj" else "upgma"
  list(
    align = if (type %in% c("dna", "protein")) list(method = "ClustalOmega") else NULL,
    filter = list(
      max_marker_missing = 0.2,
      min_maf = if (type == "snp") 0.05 else 0,
      drop_monomorphic = TRUE,
      max_sample_missing = 0.5,
      drop_empty_sites = TRUE
    ),
    distance = list(method = if (is.null(d)) NULL else default_distance(d),
                    sqrt_binary = FALSE, scale = TRUE),
    tree = list(method = tree_method, outgroup = NULL),
    bootstrap = list(B = 100, seed = 1),
    k = list(k_max = 10, gap = FALSE, seed = 1),
    groups = list(method = if (is_hierarchical(tree_method)) "tree" else "pam", k = 3,
                  ordination = "pcoa", seed = 1)
  )
}

#' Run a whole analysis from settings
#'
#' @param d A `cl_data` object as read (before filtering).
#' @param settings A list like [default_settings()]. Steps whose entry is
#'   `NULL` are skipped.
#' @param progress Optional function called with a message at each step.
#' @return A list with `data`, `dist`, `tree`, `boot`, `kres`, `groups` and
#'   `ord`.
#' @export
#' @examples
#' \donttest{
#' res <- run_analysis(clusnalyst_demo("binary"))
#' res$tree
#' }
run_analysis <- function(d, settings = default_settings(d), progress = NULL) {
  say <- function(msg) if (!is.null(progress)) progress(msg)
  out <- list()
  if (d$type %in% c("dna", "protein") && !isTRUE(d$aligned)) {
    say("Aligning sequences")
    d <- align_sequences(d, settings$align$method %||% "ClustalOmega")
  }
  if (!is.null(settings$filter)) {
    say("Filtering")
    f <- settings$filter
    d <- filter_data(d, max_marker_missing = f$max_marker_missing %||% 0.2,
                     min_maf = f$min_maf %||% 0, drop_monomorphic = f$drop_monomorphic %||% TRUE,
                     max_sample_missing = f$max_sample_missing %||% 0.5,
                     drop_empty_sites = f$drop_empty_sites %||% TRUE)
  }
  out$data <- d
  ds <- settings$distance
  if (is.null(ds)) return(out)
  say("Computing distances")
  out$dist <- compute_distance(d, ds$method, sqrt_binary = isTRUE(ds$sqrt_binary),
                               scale = !isFALSE(ds$scale))
  if (is.null(settings$tree)) return(out)
  say("Building the tree")
  out$tree <- build_tree(out$dist, settings$tree$method, outgroup = settings$tree$outgroup)
  if (!is.null(settings$bootstrap)) {
    say("Bootstrapping")
    out$boot <- bootstrap_tree(d, out$tree, attr(out$dist, "method"), B = settings$bootstrap$B,
                               seed = settings$bootstrap$seed, sqrt_binary = isTRUE(ds$sqrt_binary),
                               scale = !isFALSE(ds$scale))
  }
  if (!is.null(settings$k)) {
    say("Comparing numbers of groups")
    out$kres <- optimal_k(out$dist, out$tree, k_max = settings$k$k_max,
                          gap = isTRUE(settings$k$gap), seed = settings$k$seed)
  }
  if (!is.null(settings$groups)) {
    say("Finding groups")
    g <- settings$groups
    out$groups <- partition(out$dist, g$k, g$method, tree = out$tree, d = d, seed = g$seed)
    out$ord <- ordinate(out$dist, d, method = g$ordination %||% "pcoa")
  }
  out
}

#' Write an R script that repeats an analysis
#'
#' @param settings A settings list (see [default_settings()]).
#' @param source List describing the data: `demo` (a demo name) or `file`,
#'   plus `type` and `transposed` for tables.
#' @param meta_file Optional metadata file name.
#' @return A character string of R code.
#' @export
build_script <- function(settings, source, meta_file = NULL) {
  dp <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  L <- c(
    "# Clusnalyst analysis script",
    paste0("# Written by clusnalyst ", utils::packageVersion("clusnalyst"), " on ", format(Sys.Date())),
    "# Install once with:",
    "# install.packages(\"clusnalyst\", repos = c(\"https://ebenogoe.r-universe.dev\", \"https://cloud.r-project.org\"))",
    "",
    "library(clusnalyst)",
    ""
  )
  if (!is.null(source$demo)) {
    L <- c(L, sprintf("d <- clusnalyst_demo(%s)", dp(source$demo)))
  } else {
    args <- sprintf("\"%s\"", basename(source$file %||% "your_file.csv"))
    if (!is.null(source$type)) args <- c(args, sprintf("type = %s", dp(source$type)))
    if (!is.null(source$transposed)) args <- c(args, sprintf("transpose = %s", dp(source$transposed)))
    L <- c(L, "# Put your data file in the working directory, or give its full path",
           sprintf("d <- read_markers(%s)", paste(args, collapse = ", ")))
    if (!is.null(meta_file)) {
      L <- c(L, sprintf("d <- attach_metadata(d, read_metadata(\"%s\"))", basename(meta_file)))
    }
  }
  L <- c(L, "", paste0("settings <- ", paste(deparse(settings, width.cutoff = 70L), collapse = "\n")), "",
         "res <- run_analysis(d, settings, progress = message)", "",
         "# Distances, tree and groups",
         "dist_matrix <- as.matrix(res$dist)",
         "res$tree",
         "if (!is.null(res$groups)) table(res$groups)",
         "",
         "# Plots",
         "plot_tree(res$tree, groups = res$groups, group_title = \"Cluster\", support = res$boot$support)",
         "if (!is.null(res$kres)) plot_k(res$kres)",
         "if (!is.null(res$ord)) plot_ordination(res$ord, groups = res$groups, group_title = \"Cluster\")",
         "",
         "# Save the tree in Newick format",
         "tr <- res$tree$phylo",
         "if (!is.null(res$boot)) tr$node.label <- round(res$boot$support)",
         "ape::write.tree(tr, \"clusnalyst_tree.nwk\")",
         "")
  paste(L, collapse = "\n")
}

session_version <- 1L

#' Save and restore an analysis session
#'
#' A session file is JSON: the data, the metadata and the settings. Results
#' are recomputed when the session is opened again, so nothing executable is
#' stored.
#'
#' @param d The `cl_data` as read.
#' @param settings The settings list.
#' @param path File to write or read.
#' @return `save_session()` returns `path` invisibly; `load_session()`
#'   returns a list with `data` and `settings`.
#' @export
save_session <- function(d, settings, path) {
  x <- d$x
  dat <- list(type = d$type, source = d$source, aligned = d$aligned, seqs = as.list(d$seqs))
  if (is.data.frame(x)) {
    dat$table <- lapply(x, function(v) if (is.factor(v)) as.character(v) else v)
    dat$rownames <- rownames(x)
  } else if (!is.null(x)) {
    dat$rownames <- rownames(x)
    dat$colnames <- colnames(x)
    dat$values <- if (is.character(x)) apply(x, 1, paste, collapse = "") else as.vector(x)
    dat$mode <- if (is.character(x)) "seq" else "num"
  }
  meta <- NULL
  if (!is.null(d$meta)) meta <- list(rownames = rownames(d$meta), columns = as.list(d$meta))
  obj <- list(app = "clusnalyst", version = session_version,
              package_version = as.character(utils::packageVersion("clusnalyst")),
              saved = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              data = dat, meta = meta, settings = settings)
  writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null", na = "null", digits = NA), path)
  invisible(path)
}

#' @rdname save_session
#' @export
load_session <- function(path) {
  obj <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE, simplifyDataFrame = FALSE),
                  error = function(e) cl_abort("This is not a Clusnalyst session file."))
  if (!identical(obj$app, "clusnalyst")) cl_abort("This is not a Clusnalyst session file.")
  dat <- obj$data
  if (!dat$type %in% data_types()) cl_abort("The session file has an unknown data type.")
  seqs <- if (length(dat$seqs)) unlist(dat$seqs) else NULL
  x <- NULL
  if (!is.null(dat$table)) {
    x <- as.data.frame(lapply(dat$table, function(v) {
      v <- unlist(lapply(v, function(e) if (is.null(e)) NA else e))
      if (is.numeric(v)) v else factor(v)
    }), check.names = FALSE, stringsAsFactors = FALSE)
    rownames(x) <- unlist(dat$rownames)
  } else if (!is.null(dat$values)) {
    rn <- unlist(dat$rownames)
    if (identical(dat$mode, "seq")) {
      s <- unlist(dat$values)
      names(s) <- rn
      x <- seqs_to_matrix(s)
    } else {
      v <- as.numeric(unlist(lapply(dat$values, function(e) if (is.null(e)) NA else e)))
      x <- matrix(v, nrow = length(rn), dimnames = list(rn, unlist(dat$colnames)))
    }
  }
  d <- new_cl_data(dat$type, x = x, seqs = seqs, aligned = isTRUE(dat$aligned),
                   source = dat$source %||% list())
  if (!is.null(obj$meta)) {
    m <- as.data.frame(lapply(obj$meta$columns, function(v) {
      unlist(lapply(v, function(e) if (is.null(e)) NA else e))
    }), check.names = FALSE, stringsAsFactors = FALSE)
    rownames(m) <- unlist(obj$meta$rownames)
    d$meta <- m
  }
  list(data = d, settings = obj$settings)
}
