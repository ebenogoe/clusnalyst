svg_string <- function(p, width = 8, height = 6) {
  s <- svglite::svgstring(width = width, height = height, standalone = FALSE)
  ok <- FALSE
  on.exit(if (!ok) grDevices::dev.off())
  # ggtree's scale bar triggers a ggplot2 deprecation warning that is not ours to fix
  withCallingHandlers(print(p), warning = function(w) {
    if (grepl("size. aesthetic", conditionMessage(w))) invokeRestart("muffleWarning")
  })
  grDevices::dev.off()
  ok <- TRUE
  htmltools::HTML(as.character(s()))
}

html_table <- function(df, digits = 3, max_rows = 200) {
  if (is.null(df) || !nrow(df)) return(NULL)
  more <- nrow(df) > max_rows
  df <- utils::head(df, max_rows)
  fmt <- lapply(df, function(v) {
    if (is.numeric(v)) formatC(v, digits = digits, format = "fg", flag = "#") else as.character(v)
  })
  fmt <- lapply(fmt, function(v) { v[is.na(v) | v == "NA"] <- ""; v })
  htmltools::tagList(
    htmltools::tags$table(
      htmltools::tags$thead(htmltools::tags$tr(lapply(names(df), htmltools::tags$th))),
      htmltools::tags$tbody(lapply(seq_len(nrow(df)), function(i) {
        htmltools::tags$tr(lapply(fmt, function(v) htmltools::tags$td(v[i])))
      }))
    ),
    if (more) htmltools::tags$p(class = "note", sprintf("First %d rows shown.", max_rows))
  )
}

report_css <- "
:root { --bg:#ffffff; --band:#f6f4ff; --text:#2d2a45; --muted:#5e5a78; --brand:#5b3fd6; --line:#e6e2f5; }
* { box-sizing: border-box; }
body { margin:0; background:var(--bg); color:var(--text); font:16px/1.6 'Plus Jakarta Sans',system-ui,-apple-system,'Segoe UI',sans-serif; }
main { max-width: 60rem; margin: 0 auto; padding: 2rem 1rem 4rem; }
h1 { font-size: 2.4rem; line-height: 1.05; margin: 0 0 .25rem; font-weight: 800; letter-spacing: -.02em; }
h2 { font-size: 1.4rem; margin: 2.5rem 0 .75rem; padding-top: 1rem; border-top: 1px solid var(--line); }
.lede { color: var(--muted); margin: 0 0 1.5rem; }
.facts { display:grid; grid-template-columns: repeat(auto-fit, minmax(10rem,1fr)); gap:.75rem; margin: 1rem 0; }
.fact { background:var(--band); border-radius:12px; padding:.75rem 1rem; }
.fact b { display:block; font-size:1.5rem; color:var(--brand); }
.fact span { font-size:.85rem; color:var(--muted); }
table { border-collapse: collapse; width: 100%; font-size: .85rem; background:#fff; margin:.5rem 0 1rem; }
th, td { border-bottom: 1px solid var(--line); padding: .35rem .5rem; text-align: left; }
th { background:var(--band); }
.fig { background:#fff; border:1px solid var(--line); border-radius:14px; padding:.75rem; margin:.75rem 0; overflow:auto; }
.fig svg { max-width: 100%; height: auto; }
.fig svg text { font-family: 'Plus Jakarta Sans', system-ui, -apple-system, 'Segoe UI', sans-serif !important; }
.note { color: var(--muted); font-size: .9rem; }
dl.settings { display:grid; grid-template-columns: max-content 1fr; gap:.25rem 1rem; }
dl.settings dt { font-weight: 700; }
dl.settings dd { margin: 0; }
footer { margin-top: 3rem; color: var(--muted); font-size: .85rem; }
a { color: #5136c9; }
h1 { letter-spacing: -.04em; }
@media print { body { background:#fff; } .fig, .fact, table { border-color:#ccc; } }
"

fact <- function(value, label) {
  htmltools::div(class = "fact", htmltools::tags$b(value), htmltools::tags$span(label))
}

setting_labels <- c(
  max_marker_missing = "Maximum missing rate per marker",
  min_maf = "Minimum minor allele frequency",
  drop_monomorphic = "Remove monomorphic markers",
  max_sample_missing = "Maximum missing rate per sample",
  drop_empty_sites = "Remove all-gap alignment columns"
)

settings_list <- function(x) {
  x <- x[!vapply(x, is.null, logical(1))]
  htmltools::tags$dl(class = "settings", lapply(names(x), function(n) {
    v <- x[[n]]
    v <- if (is.logical(v)) ifelse(v, "yes", "no") else paste(format(v), collapse = ", ")
    lab <- if (n %in% names(setting_labels)) setting_labels[[n]] else gsub("_", " ", n)
    htmltools::tagList(htmltools::tags$dt(lab),
                       htmltools::tags$dd(v))
  }))
}

#' Write a self-contained HTML report
#'
#' Includes whichever results are present: data summary, quality check,
#' distances, tree, bootstrap, cluster number, groups and ordination, plus
#' the settings and software versions. Figures are embedded as SVG, so the
#' file can be opened anywhere and needs no other files.
#'
#' @param res A list with any of `raw`, `data`, `dist`, `tree`, `boot`,
#'   `kres`, `groups`, `ord`, `mantel`, as produced by the app or by
#'   [run_analysis()] (add `raw` for the unfiltered data).
#' @param settings The settings list.
#' @param path Output file.
#' @param title Report title.
#' @param tree_layout Tree layout for the figure.
#' @return `path`, invisibly.
#' @export
build_report <- function(res, settings = list(), path, title = "Clusnalyst report",
                         tree_layout = "rectangular") {
  d <- res$data %||% res$raw
  h <- htmltools::tags
  n <- n_samples(d)
  sec <- list()
  groups <- res$groups
  meta <- d$meta

  sec$data <- htmltools::tagList(
    h$h2("Data"),
    htmltools::div(class = "facts",
      fact(type_label(d$type), "data type"),
      fact(n, "samples"),
      if (!is.null(d$x)) fact(format(ncol(d$x), big.mark = ","), unit_label(d$type)),
      if (!is.null(res$raw$x) && !is.null(d$filter)) fact(format(ncol(res$raw$x), big.mark = ","), paste(unit_label(d$type), "before filtering"))
    ),
    h$p(class = "note", paste0("Source: ", d$source$file %||% "not recorded", ".",
        if (!is.null(d$source$alignment)) paste0(" Aligned with ", d$source$alignment, " (msa package).")))
  )
  if (!is.null(d$filter)) {
    f <- d$filter
    sec$qc <- htmltools::tagList(
      h$h2("Quality check"),
      h$p(sprintf("Filtering kept %d of %d samples and %s of %s %s.",
                  f$after[["samples"]], f$before[["samples"]],
                  format(f$after[["columns"]], big.mark = ","), format(f$before[["columns"]], big.mark = ","),
                  unit_label(d$type))),
      settings_list(f$settings),
      if (length(f$removed_samples)) h$p(class = "note", "Samples removed: ", paste(f$removed_samples, collapse = ", "))
    )
  }
  if (!is.null(res$dist)) {
    dv <- as.vector(res$dist)
    sec$dist <- htmltools::tagList(
      h$h2("Distances"),
      h$p("Measure: ", h$b(attr(res$dist, "label") %||% attr(res$dist, "method"))),
      htmltools::div(class = "facts",
        fact(sprintf("%.3f", min(dv)), "smallest"), fact(sprintf("%.3f", stats::median(dv)), "median"),
        fact(sprintf("%.3f", max(dv)), "largest")),
      if (n <= 120) htmltools::div(class = "fig", svg_string(plot_heatmap(res$dist,
        if (!is.null(res$tree)) tip_order(res$tree$phylo), interactive = FALSE), 8, 7))
    )
  }
  if (!is.null(res$tree)) {
    tr <- res$tree
    coph <- tryCatch(cophenetic_cor(res$dist, tr), error = function(e) NA)
    grp <- if (!is.null(groups)) stats::setNames(paste("Cluster", groups), names(groups))
    sec$tree <- htmltools::tagList(
      h$h2("Tree"),
      h$p(paste0("Method: ", tree_label(tr$method),
                 if (!is.null(tr$outgroup)) paste0(", rooted on ", paste(tr$outgroup, collapse = ", ")),
                 sprintf(". Cophenetic correlation: %.3f.", coph))),
      htmltools::div(class = "fig", svg_string(
        plot_tree(tr, layout = tree_layout, groups = grp, group_title = "Group",
                  support = res$boot$support, support_min = 50,
                  label_size = if (n > 80) 2.2 else 3.2),
        8, max(5, min(30, n * 0.2))))
    )
  }
  if (!is.null(res$boot)) {
    b <- res$boot
    s <- if (b$rooted) b$support[-1] else b$support
    sec$boot <- htmltools::tagList(
      h$h2("Bootstrap support"),
      h$p(sprintf("%d replicates (seed %s), resampling %s with replacement. %d of %d groups in the tree have at least 70%% support. Values of 50%% and above are printed on the tree.",
                  b$B, format(b$seed), unit_label(d$type), sum(s >= 70), length(s))),
      if (b$failed > 0) h$p(class = "note", sprintf("%d replicates could not be computed and were left out.", b$failed)),
      htmltools::div(class = "fig", svg_string(plot_support(b), 7, 3.5))
    )
  }
  if (!is.null(res$kres)) {
    k <- res$kres
    best <- k$table[k$table$k == k$best_silhouette, ]
    sec$k <- htmltools::tagList(
      h$h2("Number of groups"),
      h$p(paste0(sprintf("Groups were formed by %s. The average silhouette width is highest at k = %d (%.3f, %s).",
                         k$clustering, k$best_silhouette, best$silhouette, silhouette_meaning(best$silhouette)),
                 if (!is.na(k$best_gap)) sprintf(" The gap statistic suggests k = %d.", k$best_gap))),
      htmltools::div(class = "fig", svg_string(plot_k(k), 7, if (any(!is.na(k$table$gap))) 8 else 5.5)),
      html_table(k$table)
    )
  }
  if (!is.null(groups)) {
    gdf <- data.frame(sample = names(groups), cluster = unname(groups))
    if (!is.null(meta)) gdf <- cbind(gdf, meta[gdf$sample, , drop = FALSE])
    gs <- settings$groups
    sec$groups <- htmltools::tagList(
      h$h2("Groups"),
      h$p(sprintf("%d groups by %s. Group sizes: %s.", length(unique(groups)),
                  switch(gs$method %||% "tree", tree = "cutting the tree", pam = "partitioning around medoids (PAM)", kmeans = "k-means"),
                  paste(table(groups), collapse = ", "))),
      if (!is.null(res$ord)) htmltools::div(class = "fig", svg_string(
        plot_ordination(res$ord, groups = stats::setNames(paste("Cluster", groups), names(groups)),
                        group_title = "Group", labels = n <= 60), 7.5, 6)),
      html_table(gdf)
    )
  }
  if (!is.null(res$mantel)) {
    m <- res$mantel
    sec$mantel <- htmltools::tagList(
      h$h2("Mantel test"),
      h$p(sprintf("Correlation between the distances and %s: r = %.3f, p = %.3f (%d permutations, %d shared samples).",
                  m$label %||% "a second matrix", m$r, m$p, m$permutations, m$n))
    )
  }
  sec$settings <- htmltools::tagList(
    h$h2("Settings and software"),
    h$p(class = "note", "Download the R script from the app to repeat this analysis exactly."),
    h$pre(paste(utils::capture.output(utils::str(settings, give.attr = FALSE, no.list = TRUE)), collapse = "\n")),
    h$p(class = "note", sprintf("clusnalyst %s, %s, ape %s, cluster %s. Report written %s.",
        utils::packageVersion("clusnalyst"), R.version.string, utils::packageVersion("ape"),
        utils::packageVersion("cluster"), format(Sys.time(), "%d %B %Y %H:%M")))
  )
  body <- h$body(h$main(
    h$h1(title),
    h$p(class = "lede", paste0("Cluster analysis with Clusnalyst, ", format(Sys.Date(), "%d %B %Y"), ".")),
    sec,
    h$footer(htmltools::HTML(paste0(
      "Made with Clusnalyst by Ebenezer Ogoe, <a href=\"https://ebenezerogoe.com\">ebenezerogoe.com</a>. ",
      "Source: <a href=\"https://github.com/ebenogoe/clusnalyst\">github.com/ebenogoe/clusnalyst</a>."
    )))
  ))
  # Written by hand: htmltools drops <head> when a tag tree is turned into text
  head <- paste0(
    "<head>\n<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", htmltools::htmlEscape(title), "</title>\n<link rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400..800&display=swap\">\n<style>", report_css, "</style>\n</head>"
  )
  writeLines(c("<!DOCTYPE html>", "<html lang=\"en\">", head, as.character(body), "</html>"),
             path, useBytes = TRUE)
  invisible(path)
}
