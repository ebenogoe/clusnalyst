mod_distance_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    step_header(3, "Distance matrix",
                "Select a distance measure. The default is set according to the data type."),
    shiny::uiOutput(ns("body"))
  )
}

mod_distance_server <- function(id, state, goto, dark) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$body <- shiny::renderUI({
      d <- state$data
      if (is.null(d)) {
        return(bslib::card(empty_state("Finish the quality check in step 2 first.",
                                       shiny::actionButton(ns("back"), "Go to step 2", class = "btn-outline-primary"))))
      }
      ch <- distance_choices(d$type)
      sel <- shiny::isolate(state$settings$distance$method) %||% default_distance(d)
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8)),
        bslib::card(
          bslib::card_header("Distance measure"),
          shiny::selectInput(ns("method"), "Measure", ch, selected = sel, width = "100%"),
          shiny::uiOutput(ns("note")),
          if (d$type == "binary") shiny::checkboxInput(
            ns("sqrt_binary"),
            label_info("Use the square root form, sqrt(1 - S)",
                       "This is what ade4's dist.binary returns and what the 2022 version of Clusnalyst gave. The square root makes the distances Euclidean, which some ordination methods prefer. Trees keep the same shape for UPGMA but branch lengths change."),
            FALSE),
          if (d$type == "quant") shiny::checkboxInput(ns("scale"), "Scale traits to the same variance first", TRUE),
          shiny::actionButton(ns("go"), "Compute distances", class = "btn-primary w-100")
        ),
        shiny::uiOutput(ns("result"))
      )
    })

    shiny::observeEvent(input$back, goto("qc"))

    output$note <- shiny::renderUI({
      shiny::req(input$method)
      txt <- distance_notes[[input$method]]
      if (!is.null(txt)) shiny::p(class = "cl-note", txt)
    })

    shiny::observeEvent(input$go, {
      d <- state$data
      shiny::req(d)
      dm <- safely(compute_distance(d, input$method, sqrt_binary = isTRUE(input$sqrt_binary),
                                    scale = !isFALSE(input$scale)), "computing distances")
      if (is.null(dm)) return()
      reset_after(state, "distance")
      state$dist <- dm
      set_setting(state, "distance", list(method = input$method, sqrt_binary = isTRUE(input$sqrt_binary),
                                          scale = !isFALSE(input$scale)))
    })

    output$result <- shiny::renderUI({
      dm <- state$dist
      if (is.null(dm)) {
        return(bslib::card(empty_state("Pick a measure and press Compute distances.")))
      }
      v <- as.vector(dm)
      m <- as.matrix(dm)
      diag(m) <- Inf
      nearest <- which(m == min(m), arr.ind = TRUE)[1, ]
      n_zero <- sum(v == 0)
      shiny::tagList(
        bslib::card(
          bslib::card_header(class = "cl-card-head", shiny::span(attr(dm, "label")),
                             shiny::span(class = "cl-muted small", sprintf("%s pairs", fmt_n(length(v))))),
          stat_row(
            stat_tile(formatC(min(v), digits = 3, format = "f"), "smallest",
                      sprintf("%s and %s", rownames(m)[nearest[1]], colnames(m)[nearest[2]])),
            stat_tile(formatC(stats::median(v), digits = 3, format = "f"), "median"),
            stat_tile(formatC(max(v), digits = 3, format = "f"), "largest")
          ),
          if (n_zero > 0) shiny::div(class = "alert alert-info small mb-2",
                                     sprintf("%d pairs of samples are identical (distance 0). They will join at the tips of the tree.", n_zero)),
          shiny::div(class = "cl-row-end", next_button(ns("next"), "Tree construction"))
        ),
        plot_card(ns, "heat", "Distance heatmap", interactive = attr(dm, "Size") <= 120,
                  extra_menu = shiny::tagList(
                    shiny::tags$li(shiny::tags$hr(class = "dropdown-divider")),
                    shiny::tags$li(shiny::downloadLink(ns("csv"), "Matrix as CSV", class = "dropdown-item")),
                    shiny::tags$li(shiny::downloadLink(ns("xlsx"), "Matrix as Excel", class = "dropdown-item"))
                  ),
                  footer = shiny::span(class = "small cl-muted",
                                       "Samples follow the tree order once a tree is built. Darker cells indicate larger distances."))
      )
    })

    shiny::observeEvent(input[["next"]], goto("tree"))

    heat_plot <- function(dark = FALSE, interactive = TRUE) {
      dm <- state$dist
      ord <- if (!is.null(state$tree)) tip_order(state$tree$phylo) else NULL
      plot_heatmap(dm, ord, dark = dark, interactive = interactive)
    }
    heat_size <- function() {
      n <- attr(state$dist, "Size")
      s <- min(14, max(6, n * 0.14))
      c(s + 1.5, s)
    }
    output$heat <- ggiraph::renderGirafe({
      shiny::req(state$dist)
      sz <- heat_size()
      cl_girafe(heat_plot(dark()), sz[1], sz[2], dark = dark())
    })
    # Static version when there are too many samples for an interactive heatmap
    output$heat_static <- shiny::renderPlot({
      shiny::req(state$dist)
      heat_plot(dark(), interactive = FALSE)
    }, bg = "transparent", res = 96)
    plot_downloads(output, "heat", function() heat_plot(FALSE, FALSE),
                   name = function() "clusnalyst_distance_heatmap",
                   width = function() heat_size()[1], height = function() heat_size()[2])

    output$csv <- shiny::downloadHandler(
      filename = function() "clusnalyst_distances.csv",
      content = function(file) {
        m <- as.matrix(state$dist)
        utils::write.csv(data.frame(sample = rownames(m), m, check.names = FALSE), file, row.names = FALSE)
      }
    )
    output$xlsx <- shiny::downloadHandler(
      filename = function() "clusnalyst_distances.xlsx",
      content = function(file) {
        m <- as.matrix(state$dist)
        writexl::write_xlsx(list(distances = data.frame(sample = rownames(m), m, check.names = FALSE)), file)
      }
    )
    keep_downloads(output, c("csv", "xlsx"))
  })
}
