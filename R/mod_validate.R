mod_validate_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    step_header(5, "Validation",
                "Assess branch support by bootstrapping, estimate the number of groups, and test correlation with a second distance matrix."),
    shiny::uiOutput(ns("body"))
  )
}

mod_validate_server <- function(id, state, goto, lim, dark) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$body <- shiny::renderUI({
      tr <- state$tree
      if (is.null(tr)) {
        return(bslib::card(empty_state("Build a tree in step 4 first.",
                                       shiny::actionButton(ns("back"), "Go to step 4", class = "btn-outline-primary"))))
      }
      d <- state$data
      n <- attr(state$dist, "Size")
      num_meta <- if (!is.null(d$meta)) names(d$meta)[vapply(d$meta, is.numeric, logical(1))] else character()
      bslib::navset_underline(
        id = ns("tabs"),
        bslib::nav_panel(
          "Bootstrap support", value = "boot",
          bslib::layout_columns(
            class = "pt-3",
            col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8)),
            bslib::card(
              bslib::card_header("Settings"),
              shiny::p(class = "cl-note",
                       sprintf("Each replicate draws %s %s at random with replacement, recomputes the distances and rebuilds the tree. A group's support is the share of replicates that find it again. Above 70%% is usually taken as well supported.",
                               fmt_n(ncol(d$x)), unit_label(d$type))),
              shiny::numericInput(ns("B"), "Replicates", 100, min = 10, max = lim$max_bootstrap, step = 10, width = "100%"),
              shiny::numericInput(ns("seed"), label_info("Random seed", "The same seed gives the same result every time, so others can repeat your analysis."),
                                  1, min = 1, step = 1, width = "100%"),
              if (ncol(d$x) < 20) shiny::div(class = "alert alert-warning small",
                                             sprintf("Only %d %s. Bootstrap values from so few are not very informative.", ncol(d$x), unit_label(d$type))),
              shiny::actionButton(ns("run_boot"), "Run bootstrap", class = "btn-primary w-100")
            ),
            shiny::uiOutput(ns("boot_result"))
          )
        ),
        bslib::nav_panel(
          "Number of groups", value = "k",
          bslib::layout_columns(
            class = "pt-3",
            col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8)),
            bslib::card(
              bslib::card_header("Settings"),
              shiny::p(class = "cl-note",
                       if (!is.null(tr$hclust)) "The tree is cut into k groups for each k, and each split is scored."
                       else "NJ and BIONJ trees cannot be cut evenly, so the samples are split with partitioning around medoids (PAM) for each k."),
              shiny::sliderInput(ns("k_max"), "Largest number of groups to try", 3, max(3, min(20, n - 1)),
                                 min(10, n - 1), step = 1, width = "100%"),
              shiny::checkboxInput(ns("gap"), label_info("Also compute the gap statistic",
                                                         "Compares each split with splits of random data (Tibshirani and others 2001). Slower, but it can say when there is only one group."), FALSE),
              shiny::actionButton(ns("run_k"), "Compare", class = "btn-primary w-100")
            ),
            shiny::uiOutput(ns("k_result"))
          )
        ),
        bslib::nav_panel(
          "Mantel test", value = "mantel",
          bslib::layout_columns(
            class = "pt-3",
            col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8)),
            bslib::card(
              bslib::card_header("Compare with a second matrix"),
              shiny::p(class = "cl-note", "Correlates this distance matrix with a second one, such as geographic or trait distances (Mantel 1967). Significance is assessed by permuting sample labels."),
              shiny::radioButtons(ns("m_src"), "Second matrix",
                                  c(if (length(num_meta)) c("Numeric columns of the sample information" = "meta"),
                                    "Upload a file" = "file"), width = "100%"),
              shiny::conditionalPanel(
                "input.m_src == 'meta'", ns = ns,
                shiny::selectizeInput(ns("m_cols"), "Columns", num_meta, multiple = TRUE,
                                      selected = num_meta[tolower(num_meta) %in% c("latitude", "longitude", "lat", "lon", "long")],
                                      width = "100%"),
                shiny::checkboxInput(ns("m_scale"), "Scale columns first", FALSE)
              ),
              shiny::conditionalPanel(
                "input.m_src == 'file'", ns = ns,
                shiny::fileInput(ns("m_file"), label_info("File", "A square distance matrix with sample names as row and column headers, or a data table (markers or traits) that is turned into distances with the default measure for its type."),
                                 accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".vcf", ".fasta", ".fa"), width = "100%")
              ),
              shiny::numericInput(ns("perm"), "Permutations", 999, min = 99, max = 9999, step = 100, width = "100%"),
              shiny::actionButton(ns("run_mantel"), "Run Mantel test", class = "btn-primary w-100")
            ),
            shiny::uiOutput(ns("mantel_result"))
          )
        )
      )
    })

    shiny::observeEvent(input$back, goto("tree"))

    # Bootstrap ----------------------------------------------------------------
    shiny::observeEvent(input$run_boot, {
      d <- state$data
      tr <- state$tree
      shiny::req(d, tr)
      B <- min(max(10, as.integer(input$B %||% 100)), lim$max_bootstrap)
      ds <- state$settings$distance
      res <- shiny::withProgress(message = sprintf("Running %d bootstrap replicates", B), value = 0, {
        safely(bootstrap_tree(d, tr, attr(state$dist, "method"), B = B, seed = input$seed %||% 1,
                              sqrt_binary = isTRUE(ds$sqrt_binary), scale = !isFALSE(ds$scale),
                              progress = function(p) shiny::setProgress(p, detail = sprintf("%d of %d", round(p * B), B))),
               "the bootstrap")
      })
      if (is.null(res)) return()
      state$boot <- res
      set_setting(state, "bootstrap", list(B = B, seed = input$seed %||% 1))
    })

    boot_plot <- function(dark = FALSE, consensus = FALSE) {
      b <- state$boot
      if (consensus) {
        phy <- consensus_tree(b)
        phy$edge.length <- NULL
        return(plot_tree(ape::compute.brlen(phy, 1), groups = NULL, label_size = 3, dark = dark,
                         title = "Majority-rule consensus", show_scale = FALSE))
      }
      plot_tree(state$tree, support = b$support, support_min = 50, dark = dark,
                label_size = if (ape::Ntip(state$tree$phylo) > 80) 2.2 else 3.2,
                tooltip = sample_tooltips(state$tree$phylo$tip.label, state$data$meta))
    }

    output$boot_result <- shiny::renderUI({
      b <- state$boot
      if (is.null(b)) return(bslib::card(empty_state("Set the number of replicates and press Run bootstrap.")))
      s <- if (b$rooted) b$support[-1] else b$support
      shiny::tagList(
        bslib::card(
          bslib::card_header(class = "cl-card-head", shiny::span(sprintf("%d replicates, seed %s", b$B, b$seed)),
                             if (b$failed) shiny::span(class = "badge text-bg-warning", sprintf("%d failed", b$failed))),
          stat_row(
            stat_tile(sprintf("%d of %d", sum(s >= 70), length(s)), "groups with 70% or more"),
            stat_tile(sprintf("%.0f%%", stats::median(s)), "median support")
          ),
          shiny::p(class = "cl-muted small mb-0", "Support now shows on the tree in step 4 as well. Use the Newick download there to take it into FigTree, iTOL or MEGA.")
        ),
        plot_card(ns, "boot_tree", "Tree with support of 50% or more",
                  footer = shiny::checkboxInput(ns("consensus"), "Show the majority-rule consensus of the replicates instead", FALSE)),
        plot_card(ns, "support", "How support is spread")
      )
    })

    boot_size <- function() {
      n <- ape::Ntip(state$tree$phylo)
      c(8, min(40, max(5, n * 0.17 + 1)))
    }
    output$boot_tree <- ggiraph::renderGirafe({
      shiny::req(state$boot)
      sz <- boot_size()
      cl_girafe(boot_plot(dark(), isTRUE(input$consensus)), sz[1], sz[2], dark = dark())
    })
    plot_downloads(output, "boot_tree", function() boot_plot(FALSE, isTRUE(input$consensus)),
                   name = function() "clusnalyst_bootstrap_tree",
                   width = function() boot_size()[1], height = function() boot_size()[2])
    output$support <- ggiraph::renderGirafe({
      shiny::req(state$boot)
      cl_girafe(plot_support(state$boot, dark()), 9, 3.2, dark = dark())
    })
    plot_downloads(output, "support", function() plot_support(state$boot), name = function() "clusnalyst_support",
                   width = 7, height = 3.5)

    # Number of groups -----------------------------------------------------------
    shiny::observeEvent(input$run_k, {
      shiny::req(state$dist)
      res <- shiny::withProgress(message = "Comparing numbers of groups", value = 0.4, {
        safely(optimal_k(state$dist, state$tree, k_max = input$k_max, gap = isTRUE(input$gap)),
               "comparing numbers of groups")
      })
      if (is.null(res)) return()
      state$kres <- res
      state$k_suggest <- res$best_silhouette
      set_setting(state, "k", list(k_max = input$k_max, gap = isTRUE(input$gap), seed = 1))
    })

    output$k_result <- shiny::renderUI({
      k <- state$kres
      if (is.null(k)) return(bslib::card(empty_state("Press Compare to score splits into 2 groups and up.")))
      best <- k$table[k$table$k == k$best_silhouette, ]
      shiny::tagList(
        bslib::card(
          bslib::card_header("What the scores say"),
          stat_row(
            stat_tile(k$best_silhouette, "groups by silhouette", sprintf("width %.2f", best$silhouette)),
            if (!is.na(k$best_gap)) stat_tile(k$best_gap, "groups by gap statistic",
                                              if (k$best_gap == 1) "no clear grouping")
          ),
          shiny::p(sprintf("The average silhouette width peaks at %d groups (%.2f). By the usual reading (Kaufman and Rousseeuw 1990) that is %s.",
                           k$best_silhouette, best$silhouette, silhouette_meaning(best$silhouette))),
          if (state$data$type %in% c("snp", "binary") && best$silhouette < 0.5) {
            shiny::p(class = "cl-muted small",
                     "Silhouette widths between individual genotypes are typically low even when groups are real, because individuals differ at many markers. Interpret the peak together with the gap statistic and the ordination in step 6.")
          },
          shiny::div(class = "cl-row-end",
                     shiny::actionButton(ns("use_k"), sprintf("Use %d groups in step 6", k$best_silhouette), class = "btn-primary cl-next"))
        ),
        plot_card(ns, "k_plot", "Scores for each number of groups")
      )
    })
    k_height <- function() if (!is.null(state$kres) && any(!is.na(state$kres$table$gap))) 8 else 5.5
    output$k_plot <- ggiraph::renderGirafe({
      shiny::req(state$kres)
      cl_girafe(plot_k(state$kres, dark()), 7, k_height(), dark = dark())
    })
    plot_downloads(output, "k_plot", function() plot_k(state$kres), name = function() "clusnalyst_number_of_groups",
                   width = 7, height = k_height)
    shiny::observeEvent(input$use_k, {
      state$k_suggest <- state$kres$best_silhouette
      goto("groups")
    })

    # Mantel ----------------------------------------------------------------------
    shiny::observeEvent(input$run_mantel, {
      shiny::req(state$dist)
      second <- if (identical(input$m_src, "meta")) {
        if (!length(input$m_cols)) {
          shiny::showNotification("Choose at least one column.", type = "warning")
          return()
        }
        safely(metadata_distance(state$data$meta, input$m_cols, scale = isTRUE(input$m_scale)), "building the second matrix")
      } else {
        if (is.null(input$m_file)) {
          shiny::showNotification("Choose a file first.", type = "warning")
          return()
        }
        safely(read_second_matrix(input$m_file$datapath, input$m_file$name), "reading the second matrix")
      }
      if (is.null(second)) return()
      res <- shiny::withProgress(message = "Running the Mantel test", value = 0.5, {
        safely(mantel_test(state$dist, second, permutations = input$perm %||% 999), "the Mantel test")
      })
      if (is.null(res)) return()
      res$label <- attr(second, "label") %||% "the second matrix"
      state$mantel <- res
    })

    output$mantel_result <- shiny::renderUI({
      m <- state$mantel
      if (is.null(m)) return(bslib::card(empty_state("Choose a second matrix and press Run Mantel test.")))
      shiny::tagList(
        bslib::card(
          bslib::card_header(m$label),
          stat_row(stat_tile(sprintf("%.3f", m$r), "Mantel r"),
                   stat_tile(if (m$p < 0.001) "< 0.001" else sprintf("%.3f", m$p), "p-value"),
                   stat_tile(m$n, "shared samples")),
          shiny::p(class = "small mb-0",
                   if (m$p < 0.05) sprintf("The two sets of distances are related (r = %.2f, one-sided test, %d permutations).", m$r, m$permutations)
                   else "No evidence that the two sets of distances are related.")
        ),
        plot_card(ns, "mantel_plot", "Pairs of distances")
      )
    })
    output$mantel_plot <- ggiraph::renderGirafe({
      shiny::req(state$mantel)
      cl_girafe(plot_mantel(state$mantel, c(attr(state$dist, "label"), state$mantel$label), dark()), 7, 5, dark = dark())
    })
    plot_downloads(output, "mantel_plot", function() plot_mantel(state$mantel, c(attr(state$dist, "label"), state$mantel$label)),
                   name = function() "clusnalyst_mantel", width = 7, height = 5)
  })
}
