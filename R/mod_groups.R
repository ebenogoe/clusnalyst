mod_groups_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    step_header(6, "Groups and ordination",
                "Partition samples into groups and display them on principal coordinates or principal components."),
    shiny::uiOutput(ns("body"))
  )
}

mod_groups_server <- function(id, state, goto, dark) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$body <- shiny::renderUI({
      dm <- state$dist
      if (is.null(dm)) {
        return(bslib::card(empty_state("Compute distances in step 3 first.",
                                       shiny::actionButton(ns("back"), "Go to step 3", class = "btn-outline-primary"))))
      }
      d <- state$data
      tr <- state$tree
      n <- attr(dm, "Size")
      methods <- c(if (!is.null(tr$hclust)) c("Cut the tree" = "tree"),
                   "Partitioning around medoids (PAM)" = "pam",
                   if (has_numeric(d)) c("k-means" = "kmeans"))
      ords <- c("Principal coordinates (PCoA)" = "pcoa", if (has_numeric(d)) c("Principal components (PCA)" = "pca"))
      k0 <- shiny::isolate(state$k_suggest %||% state$settings$groups$k %||% 3)
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8)),
        shiny::div(
          bslib::card(
            bslib::card_header("Groups"),
            shiny::selectInput(ns("method"),
                               label_info("Method",
                                          shiny::p(shiny::tags$b("Cut the tree: "), "groups are the branches left when the tree is cut at one height. Matches the tree exactly."),
                                          shiny::p(shiny::tags$b("PAM: "), "picks k real samples as centres and assigns every sample to the nearest (Kaufman and Rousseeuw 1990). Works with any distance."),
                                          shiny::p(shiny::tags$b("k-means: "), "the classic method for numeric data. Uses the markers or scaled traits directly, not the distances.")),
                               methods, width = "100%"),
            shiny::sliderInput(ns("k"), "Number of groups", 2, max(2, min(20, n - 1)), min(k0, n - 1), step = 1, width = "100%"),
            if (!is.null(state$kres)) shiny::p(class = "cl-muted small",
                                               sprintf("Step 5 suggested %d groups.", state$kres$best_silhouette)),
            shiny::actionButton(ns("go"), "Find groups", class = "btn-primary w-100")
          ),
          bslib::card(
            bslib::card_header("Map"),
            shiny::selectInput(ns("ord"),
                               label_info("Method",
                                          "PCoA places samples so that their distances on the map match the distance matrix as closely as possible, so it works for every data type. PCA works on the numbers themselves and is offered for markers and numeric traits."),
                               ords, width = "100%"),
            shiny::selectInput(ns("axes"), "Axes", c("1 and 2" = "1,2", "1 and 3" = "1,3", "2 and 3" = "2,3"), width = "100%"),
            shiny::selectInput(ns("colour_by"), "Colour by",
                               c("Groups" = "groups", meta_group_choices(d$meta)), width = "100%"),
            shiny::checkboxInput(ns("labels"), "Show sample names", n <= 40),
            shiny::checkboxInput(ns("ellipses"), "Draw 95% ellipses", TRUE)
          )
        ),
        shiny::uiOutput(ns("result"))
      )
    })

    shiny::observeEvent(input$back, goto("distance"))

    shiny::observeEvent(input$go, {
      dm <- state$dist
      shiny::req(dm)
      cl <- safely(partition(dm, input$k, input$method, tree = state$tree, d = state$data), "finding groups")
      if (is.null(cl)) return()
      ord <- safely(ordinate(dm, state$data, method = input$ord %||% "pcoa"), "the ordination")
      if (is.null(ord)) return()
      state$groups <- cl
      state$ord <- ord
      set_setting(state, "groups", list(method = input$method, k = input$k, ordination = input$ord %||% "pcoa", seed = 1))
    })

    # Recompute only the map when its method changes
    shiny::observeEvent(input$ord, {
      shiny::req(state$groups, state$dist)
      ord <- safely(ordinate(state$dist, state$data, method = input$ord), "the ordination")
      if (!is.null(ord)) {
        state$ord <- ord
        g <- state$settings$groups
        if (!is.null(g)) {
          g$ordination <- input$ord
          set_setting(state, "groups", g)
        }
      }
    }, ignoreInit = TRUE)

    colour <- shiny::reactive({
      cb <- input$colour_by %||% "groups"
      if (startsWith(cb, "meta:")) {
        col <- sub("^meta:", "", cb)
        m <- state$data$meta
        return(list(g = stats::setNames(as.character(m[[col]]), rownames(m)), title = col))
      }
      list(g = stats::setNames(paste("Cluster", state$groups), names(state$groups)), title = "Cluster")
    })

    ord_plot <- function(dark = FALSE) {
      ax <- as.integer(strsplit(input$axes %||% "1,2", ",")[[1]])
      ax <- ax[ax <= ncol(state$ord$scores)]
      if (length(ax) < 2) ax <- c(1, 2)
      g <- colour()
      plot_ordination(state$ord, groups = g$g, group_title = g$title, axes = ax,
                      labels = isTRUE(input$labels), ellipses = isTRUE(input$ellipses),
                      tooltip = sample_tooltips(rownames(state$ord$scores), state$data$meta, state$groups),
                      dark = dark)
    }

    membership <- shiny::reactive({
      cl <- state$groups
      shiny::req(cl)
      df <- data.frame(sample = names(cl), cluster = unname(cl))
      sc <- state$ord$scores[df$sample, seq_len(min(3, ncol(state$ord$scores))), drop = FALSE]
      names(sc) <- paste0(if (state$ord$method == "pcoa") "PCo" else "PC", seq_len(ncol(sc)))
      df <- cbind(df, sc)
      if (!is.null(state$data$meta)) df <- cbind(df, state$data$meta[df$sample, , drop = FALSE])
      df
    })

    output$result <- shiny::renderUI({
      cl <- state$groups
      if (is.null(cl)) return(bslib::card(empty_state("Choose a method and a number of groups, then press Find groups.")))
      sizes <- table(cl)
      sil <- tryCatch(mean(cluster::silhouette(cl, state$dist)[, "sil_width"]), error = function(e) NA)
      shiny::tagList(
        bslib::card(
          bslib::card_header(class = "cl-card-head",
                             shiny::span(sprintf("%d groups", length(sizes))),
                             shiny::span(class = "cl-muted small", paste("Sizes:", paste(sizes, collapse = ", ")))),
          stat_row(
            stat_tile(length(sizes), "groups"),
            stat_tile(if (is.na(sil)) "n/a" else sprintf("%.2f", sil), "average silhouette width"),
            stat_tile(sprintf("%.1f%%", sum(utils::head(state$ord$variance, 2))), "variation on axes 1 and 2")
          ),
          if (state$ord$negative_eigen > 0.05) shiny::p(class = "cl-muted small",
            sprintf("%.0f%% of the PCoA eigenvalues are negative, so this distance is not fully Euclidean and the map is an approximation.", 100 * state$ord$negative_eigen)),
          shiny::div(class = "cl-row-end",
                     shiny::span(class = "cl-muted small", "The tree in step 4 can be coloured by these groups."),
                     next_button(ns("next"), "Export"))
        ),
        plot_card(ns, "map", if (state$ord$method == "pcoa") "Principal coordinates" else "Principal components"),
        bslib::layout_columns(
          col_widths = 12,
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(class = "cl-card-head", shiny::span("Who is in which group"),
                               shiny::div(class = "d-flex gap-2",
                                          shiny::downloadButton(ns("csv"), "CSV", class = "btn-sm btn-outline-secondary"),
                                          shiny::downloadButton(ns("xlsx"), "Excel", class = "btn-sm btn-outline-secondary"))),
            reactable::reactableOutput(ns("members"))
          ),
          plot_card(ns, "scree", "Variation per axis")
        )
      )
    })

    output$map <- ggiraph::renderGirafe({
      shiny::req(state$ord, state$groups)
      cl_girafe(ord_plot(dark()), 8, 6.5, dark = dark())
    })
    plot_downloads(output, "map", function() ord_plot(FALSE), name = function() "clusnalyst_ordination", width = 8, height = 6.5)
    output$scree <- ggiraph::renderGirafe({
      shiny::req(state$ord)
      cl_girafe(plot_scree(state$ord, dark = dark()), 9, 3, dark = dark())
    })
    plot_downloads(output, "scree", function() plot_scree(state$ord), name = function() "clusnalyst_scree", width = 7, height = 3.5)
    output$members <- reactable::renderReactable(cl_reactable(membership(), digits = 3))
    output$csv <- shiny::downloadHandler(
      filename = function() "clusnalyst_groups.csv",
      content = function(file) utils::write.csv(membership(), file, row.names = FALSE)
    )
    output$xlsx <- shiny::downloadHandler(
      filename = function() "clusnalyst_groups.xlsx",
      content = function(file) writexl::write_xlsx(list(groups = membership()), file)
    )
    shiny::observeEvent(input[["next"]], goto("report"))
  })
}
