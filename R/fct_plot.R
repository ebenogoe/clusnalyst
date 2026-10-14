#' Colours for groups
#'
#' The Okabe and Ito palette (safe for colour-blind readers), without its
#' yellow and black, and HCL colours beyond eight groups.
#' @param n Number of colours.
#' @return Character vector of hex colours.
#' @export
cl_palette <- function(n) {
  oi <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#882255", "#7F7F7F")
  if (n <= length(oi)) oi[seq_len(n)] else grDevices::hcl.colors(n, "Dark 3")
}

brand <- list(accent = "#5b3fd6", accent_dark = "#a594ff", text = "#2d2a45",
              text_dark = "#e2dff2", muted = "#5e5a78", muted_dark = "#a9a4c4",
              line = "#ebe8f6", line_dark = "#2c2945", coral = "#ff7a59")

#' ggplot2 theme used by Clusnalyst
#' @param base_size Base font size.
#' @param dark Use colours for a dark background.
#' @return A ggplot2 theme.
#' @export
theme_clusnalyst <- function(base_size = 12, dark = FALSE) {
  txt <- if (dark) brand$text_dark else brand$text
  mut <- if (dark) brand$muted_dark else brand$muted
  grid <- if (dark) brand$line_dark else brand$line
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = txt),
      axis.text = ggplot2::element_text(colour = mut),
      plot.title = ggplot2::element_text(face = "bold", size = base_size * 1.2),
      plot.title.position = "plot",
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = grid, linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(colour = txt, face = "bold", hjust = 0),
      legend.position = "bottom"
    )
}

tree_layouts <- function() {
  c("Rectangular" = "rectangular", "Circular" = "circular", "Fan" = "fan",
    "Unrooted" = "unrooted", "Slanted" = "slanted")
}

#' Plot a tree
#'
#' @param tree A `cl_tree` or an `ape` `phylo`.
#' @param layout One of `"rectangular"`, `"circular"`, `"fan"`,
#'   `"unrooted"` or `"slanted"`.
#' @param groups Optional named vector (names = sample names) used to
#'   colour the tips.
#' @param group_title Legend title for `groups`.
#' @param support Optional bootstrap support, one value per internal node.
#' @param support_min Only show support at or above this value.
#' @param tooltip Optional named character vector of hover text per tip.
#' @param label_size Size of the tip labels.
#' @param title Plot title.
#' @param dark Colours for a dark background.
#' @param show_scale Draw a branch-length scale bar.
#' @return A ggplot object (interactive with [ggiraph::girafe()]).
#' @export
plot_tree <- function(tree, layout = "rectangular", groups = NULL, group_title = "Group",
                      support = NULL, support_min = 0, tooltip = NULL, label_size = 3.5,
                      title = NULL, dark = FALSE, show_scale = TRUE) {
  phy <- if (inherits(tree, "cl_tree")) tree$phylo else tree
  rooted <- if (inherits(tree, "cl_tree")) tree$rooted else ape::is.rooted(phy)
  ntip <- ape::Ntip(phy)
  lay <- switch(layout, unrooted = "equal_angle", layout)
  txt <- if (dark) brand$text_dark else brand$text
  line_col <- if (dark) "#8d88ab" else "#3d3a58"
  p <- ggtree::ggtree(phy, layout = lay, colour = line_col, linewidth = 0.5)
  tips <- phy$tip.label
  info <- data.frame(label = tips, stringsAsFactors = FALSE)
  info$cl_group <- if (!is.null(groups)) as.character(groups[tips]) else NA_character_
  info$cl_tip <- if (!is.null(tooltip)) unname(tooltip[tips]) else tips
  info$cl_tip[is.na(info$cl_tip)] <- info$label[is.na(info$cl_tip)]
  p <- ggtree::`%<+%`(p, info)
  has_groups <- !is.null(groups) && any(!is.na(info$cl_group))
  tip_aes <- if (has_groups) ggplot2::aes(colour = .data$cl_group) else ggplot2::aes()
  lab_args <- list(mapping = tip_aes, size = label_size, offset = 0, show.legend = FALSE)
  if (!has_groups) lab_args$colour <- txt
  lab_args$hjust <- if (layout == "unrooted") -0.15 else -0.1
  p <- p + do.call(ggtree::geom_tiplab, lab_args)
  pt_args <- list(
    mapping = utils::modifyList(
      ggplot2::aes(x = .data$x, y = .data$y, tooltip = .data$cl_tip, data_id = .data$label),
      if (has_groups) ggplot2::aes(colour = .data$cl_group) else ggplot2::aes()
    ),
    data = function(df) df[df$isTip, ],
    size = 2.2,
    show.legend = has_groups
  )
  if (!has_groups) pt_args$colour <- txt
  p <- p + do.call(ggiraph::geom_point_interactive, pt_args)
  if (has_groups) {
    lv <- sort(unique(stats::na.omit(info$cl_group)))
    p <- p + ggplot2::scale_colour_manual(values = stats::setNames(cl_palette(length(lv)), lv),
                                          name = group_title, na.value = "grey60")
  }
  if (!is.null(support)) {
    p$data$cl_support <- NA_real_
    internal <- !p$data$isTip
    p$data$cl_support[internal] <- support[p$data$node[internal] - ntip]
    if (rooted) p$data$cl_support[p$data$node == ntip + 1] <- NA
    p$data$cl_support_lab <- ifelse(!is.na(p$data$cl_support) & p$data$cl_support >= support_min,
                                    round(p$data$cl_support), NA)
    p <- p + ggplot2::geom_text(
      data = function(df) df[!is.na(df$cl_support_lab), ],
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$cl_support_lab),
      size = label_size * 0.8, colour = if (dark) brand$accent_dark else brand$accent,
      hjust = if (layout %in% c("rectangular", "slanted")) 1.2 else 0.5,
      vjust = -0.5, fontface = "bold"
    )
  }
  if (layout %in% c("rectangular", "slanted")) {
    p <- p + ggtree::hexpand(0.25)
    if (show_scale) p <- p + ggtree::geom_treescale(color = txt, fontsize = label_size, linesize = 0.4)
  } else if (layout == "unrooted") {
    p <- p + ggtree::hexpand(0.15, direction = 1) + ggtree::hexpand(0.15, direction = -1) +
      ggtree::vexpand(0.08, direction = 1) + ggtree::vexpand(0.08, direction = -1)
  } else {
    p <- p + ggtree::hexpand(0.25)
  }
  p <- p + ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.text = ggplot2::element_text(colour = txt),
    legend.title = ggplot2::element_text(colour = txt, face = "bold"),
    plot.title = ggplot2::element_text(colour = txt, face = "bold", size = 15),
    legend.position = "bottom"
  )
  if (!is.null(title) && nzchar(title)) p <- p + ggplot2::ggtitle(title)
  p
}

#' Heatmap of a distance matrix
#' @param dist A `dist` object.
#' @param order Optional sample order (for example the tree's tip order).
#' @param dark Colours for a dark background.
#' @param interactive Add hover text to each cell.
#' @return A ggplot object.
#' @export
plot_heatmap <- function(dist, order = NULL, dark = FALSE, interactive = TRUE) {
  m <- as.matrix(dist)
  order <- order %||% labels(dist)
  m <- m[order, order]
  n <- nrow(m)
  df <- data.frame(
    a = factor(rep(rownames(m), times = n), levels = rownames(m)),
    b = factor(rep(colnames(m), each = n), levels = rev(colnames(m))),
    d = as.vector(m)
  )
  df$tip <- sprintf("%s and %s: %.4f", df$a, df$b, df$d)
  # Leave the diagonal blank so the colour scale shows differences between samples
  df$d[as.character(df$a) == as.character(df$b)] <- NA
  geom <- if (interactive) {
    ggiraph::geom_tile_interactive(ggplot2::aes(tooltip = .data$tip, data_id = .data$tip))
  } else {
    ggplot2::geom_tile()
  }
  show_lab <- n <= 60
  ggplot2::ggplot(df, ggplot2::aes(.data$a, .data$b, fill = .data$d)) +
    geom +
    ggplot2::scale_fill_viridis_c(option = "mako", direction = -1, name = "Distance", na.value = "transparent") +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_clusnalyst(dark = dark) +
    ggplot2::theme(
      axis.text.x = if (show_lab) ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7) else ggplot2::element_blank(),
      axis.text.y = if (show_lab) ggplot2::element_text(size = 7) else ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      legend.position = "right"
    )
}

#' Plot an ordination
#' @param ord A `cl_ord` from [ordinate()].
#' @param groups Optional named vector of groups for colour.
#' @param group_title Legend title.
#' @param axes Which two axes to plot.
#' @param labels Show sample names.
#' @param ellipses Draw 95% ellipses for groups with at least four samples.
#' @param tooltip Optional named character vector of hover text.
#' @param dark Colours for a dark background.
#' @return A ggplot object.
#' @export
plot_ordination <- function(ord, groups = NULL, group_title = "Group", axes = c(1, 2),
                            labels = FALSE, ellipses = TRUE, tooltip = NULL, dark = FALSE) {
  sc <- ord$scores
  if (ncol(sc) < max(axes)) cl_abort("Only ", ncol(sc), " axes are available.")
  df <- data.frame(id = rownames(sc), x = sc[[axes[1]]], y = sc[[axes[2]]])
  df$g <- if (!is.null(groups)) as.character(groups[df$id]) else "All"
  df$tip <- if (!is.null(tooltip)) unname(tooltip[df$id]) else df$id
  df$tip[is.na(df$tip)] <- df$id[is.na(df$tip)]
  lab <- if (ord$method == "pcoa") "PCo" else "PC"
  xl <- sprintf("%s%d (%.1f%%)", lab, axes[1], ord$variance[axes[1]])
  yl <- sprintf("%s%d (%.1f%%)", lab, axes[2], ord$variance[axes[2]])
  has_groups <- !is.null(groups)
  lv <- sort(unique(df$g))
  p <- ggplot2::ggplot(df, ggplot2::aes(.data$x, .data$y, colour = .data$g)) +
    ggplot2::geom_hline(yintercept = 0, colour = if (dark) brand$line_dark else brand$line) +
    ggplot2::geom_vline(xintercept = 0, colour = if (dark) brand$line_dark else brand$line)
  if (ellipses && has_groups) {
    big <- names(which(table(df$g) >= 4))
    if (length(big)) {
      p <- p + ggplot2::stat_ellipse(data = df[df$g %in% big, ], type = "t", level = 0.95,
                                     linewidth = 0.5, linetype = 2, show.legend = FALSE)
    }
  }
  p <- p + ggiraph::geom_point_interactive(ggplot2::aes(tooltip = .data$tip, data_id = .data$id),
                                           size = 2.8, alpha = 0.9)
  if (labels) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$id), size = 3, vjust = -0.9,
                                check_overlap = TRUE, show.legend = FALSE)
  }
  p + ggplot2::scale_colour_manual(
      values = if (has_groups) stats::setNames(cl_palette(length(lv)), lv) else c(All = if (dark) brand$accent_dark else brand$accent),
      name = group_title, guide = if (has_groups) "legend" else "none") +
    ggplot2::labs(x = xl, y = yl) +
    theme_clusnalyst(dark = dark)
}

#' Scree plot of an ordination
#' @inheritParams plot_ordination
#' @param n Number of axes to show.
#' @return A ggplot object.
#' @export
plot_scree <- function(ord, n = 10, dark = FALSE) {
  v <- utils::head(ord$variance, n)
  df <- data.frame(axis = factor(seq_along(v)), var = v)
  ggplot2::ggplot(df, ggplot2::aes(.data$axis, .data$var)) +
    ggplot2::geom_col(fill = if (dark) brand$accent_dark else brand$accent, width = 0.7) +
    ggplot2::labs(x = "Axis", y = "Variance explained (%)") +
    theme_clusnalyst(dark = dark)
}

#' Plot the cluster-number diagnostics
#' @param kres Result of [optimal_k()].
#' @param dark Colours for a dark background.
#' @return A ggplot object.
#' @export
plot_k <- function(kres, dark = FALSE) {
  t <- kres$table
  long <- rbind(
    data.frame(k = t$k, value = t$silhouette, lo = NA, hi = NA, what = "Average silhouette width (higher is better)"),
    data.frame(k = t$k, value = t$wss, lo = NA, hi = NA, what = "Within-group sum of squares (look for the elbow)")
  )
  if (any(!is.na(t$gap))) {
    long <- rbind(long, data.frame(k = t$k, value = t$gap, lo = t$gap - t$gap_se,
                                   hi = t$gap + t$gap_se, what = "Gap statistic (higher is better)"))
  }
  long$what <- factor(long$what, levels = unique(long$what))
  best <- data.frame(k = kres$best_silhouette, what = levels(long$what)[1])
  col <- if (dark) brand$accent_dark else brand$accent
  p <- ggplot2::ggplot(long, ggplot2::aes(.data$k, .data$value)) +
    ggplot2::geom_vline(data = best, ggplot2::aes(xintercept = .data$k), linetype = 2,
                        colour = if (dark) brand$muted_dark else brand$muted) +
    ggplot2::geom_line(colour = col, linewidth = 0.8) +
    ggiraph::geom_point_interactive(ggplot2::aes(tooltip = sprintf("k = %d: %.3f", .data$k, .data$value)),
                                    colour = col, size = 2.6)
  if (any(!is.na(long$lo))) {
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lo, ymax = .data$hi),
                                    width = 0.15, colour = col, na.rm = TRUE)
  }
  p + ggplot2::facet_wrap(~what, scales = "free_y", ncol = 1) +
    ggplot2::scale_x_continuous(breaks = t$k) +
    ggplot2::labs(x = "Number of groups (k)", y = NULL) +
    theme_clusnalyst(dark = dark)
}

#' Plot quality-check summaries
#' @param qc Result of [qc_summary()].
#' @param dark Colours for a dark background.
#' @param thresholds Optional named list with `marker_missing` and `maf`
#'   cut-offs to draw.
#' @return A ggplot object.
#' @export
plot_qc <- function(qc, dark = FALSE, thresholds = NULL) {
  col <- if (dark) brand$accent_dark else brand$accent
  if (qc$type %in% c("binary", "snp")) {
    long <- rbind(
      data.frame(value = qc$markers$missing, what = "Missing data per marker"),
      data.frame(value = qc$samples$missing, what = "Missing data per sample"),
      data.frame(value = qc$markers$maf, what = if (qc$type == "snp") "Minor allele frequency" else "Minor band frequency")
    )
    if (qc$type == "snp") long <- rbind(long, data.frame(value = qc$markers$heterozygosity, what = "Heterozygosity per marker"))
  } else if (qc$type == "quant") {
    long <- rbind(
      data.frame(value = qc$markers$missing, what = "Missing data per trait"),
      data.frame(value = qc$samples$missing, what = "Missing data per sample")
    )
  } else {
    long <- data.frame(value = qc$lengths, what = "Sequence length (without gaps)")
    if (!is.null(qc$samples$gaps)) long <- rbind(long, data.frame(value = qc$samples$gaps, what = "Share of gaps per sequence"))
  }
  long <- long[!is.na(long$value), ]
  long$what <- factor(long$what, levels = unique(long$what))
  p <- ggplot2::ggplot(long, ggplot2::aes(.data$value)) +
    ggplot2::geom_histogram(bins = 30, fill = col, colour = NA) +
    ggplot2::facet_wrap(~what, scales = "free", ncol = 2) +
    ggplot2::labs(x = NULL, y = "Count") +
    theme_clusnalyst(dark = dark)
  if (!is.null(thresholds)) {
    th <- data.frame(
      what = c("Missing data per marker", "Missing data per trait", "Missing data per sample",
               "Minor allele frequency", "Minor band frequency"),
      v = c(thresholds$marker_missing %||% NA, thresholds$marker_missing %||% NA,
            thresholds$sample_missing %||% NA, thresholds$maf %||% NA, thresholds$maf %||% NA)
    )
    th <- th[th$what %in% levels(long$what) & !is.na(th$v), ]
    if (nrow(th)) {
      th$what <- factor(th$what, levels = levels(long$what))
      p <- p + ggplot2::geom_vline(data = th, ggplot2::aes(xintercept = .data$v),
                                   linetype = 2, colour = brand$coral)
    }
  }
  p
}

#' Plot bootstrap support values
#' @param boot Result of [bootstrap_tree()].
#' @param dark Colours for a dark background.
#' @return A ggplot object.
#' @export
plot_support <- function(boot, dark = FALSE) {
  s <- if (boot$rooted) boot$support[-1] else boot$support
  bins <- cut(s, breaks = seq(0, 100, 10), include.lowest = TRUE, right = FALSE)
  # [90, 100] is closed so 100% support lands in the last bar
  df <- as.data.frame(table(bins))
  df$mid <- seq(5, 95, 10)
  ggplot2::ggplot(df, ggplot2::aes(.data$mid, .data$Freq)) +
    ggplot2::geom_col(width = 9.2, fill = if (dark) brand$accent_dark else brand$accent) +
    ggplot2::geom_vline(xintercept = 70, linetype = 2, colour = brand$coral) +
    ggplot2::scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    ggplot2::labs(x = "Bootstrap support (%)", y = "Number of groups in the tree") +
    theme_clusnalyst(dark = dark)
}

#' Plot two distance matrices against each other
#' @param mantel Result of [mantel_test()].
#' @param labels Axis labels.
#' @param dark Colours for a dark background.
#' @return A ggplot object.
#' @export
plot_mantel <- function(mantel, labels = c("Distance 1", "Distance 2"), dark = FALSE) {
  ggplot2::ggplot(mantel$pairs, ggplot2::aes(.data$d2, .data$d1)) +
    ggplot2::geom_point(alpha = 0.35, size = 1.4, colour = if (dark) brand$accent_dark else brand$accent) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = brand$coral, linewidth = 0.8) +
    ggplot2::labs(x = labels[2], y = labels[1],
                  subtitle = sprintf("Mantel r = %.3f, p = %.3f (%d permutations)",
                                     mantel$r, mantel$p, mantel$permutations)) +
    theme_clusnalyst(dark = dark)
}
