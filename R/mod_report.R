mod_report_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    step_header(7, "Export",
                "Download the HTML report, an R script that reproduces the analysis, or a session file."),
    shiny::uiOutput(ns("body"))
  )
}

mod_report_server <- function(id, state, goto) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$body <- shiny::renderUI({
      if (is.null(state$tree)) {
        return(bslib::card(empty_state("Build a tree in step 4 first.",
                                       shiny::actionButton(ns("back"), "Go to step 4", class = "btn-outline-primary"))))
      }
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
        shiny::div(
          bslib::card(
            bslib::card_header(class = "cl-card-head", shiny::span("Report"), shiny::span(class = "cl-muted small", "HTML, one file")),
            shiny::p("Self-contained HTML file with the data summary, figures, tables and settings. Opens in any browser and prints to PDF."),
            shiny::textInput(ns("title"), "Title", "Clusnalyst report", width = "100%"),
            shiny::selectInput(ns("layout"), "Tree layout in the report", tree_layouts()[c(1, 2, 4)], width = "100%"),
            shiny::downloadButton(ns("report"), "Download report", class = "btn-primary w-100")
          ),
          bslib::card(
            bslib::card_header(class = "cl-card-head", shiny::span("R script"), shiny::span(class = "cl-muted small", ".R")),
            shiny::p("Reproduces the analysis with the clusnalyst package, using the same settings and seeds. Run it from the folder containing your data file."),
            shiny::downloadButton(ns("script"), "Download R script", class = "btn-outline-primary w-100")
          ),
          bslib::card(
            bslib::card_header(class = "cl-card-head", shiny::span("Session file"), shiny::span(class = "cl-muted small", ".json")),
            shiny::p("Data and settings, for reopening under Load data, Resume. No data are stored on the server."),
            shiny::downloadButton(ns("session"), "Download session file", class = "btn-outline-primary w-100")
          )
        ),
        shiny::div(
          bslib::card(
            bslib::card_header("What goes in"),
            shiny::uiOutput(ns("checklist"))
          ),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header("The R script"),
            shiny::textOutput(ns("script_text"), container = function(...) shiny::tags$pre(class = "cl-code", ...))
          )
        )
      )
    })

    shiny::observeEvent(input$back, goto("tree"))

    settings <- shiny::reactive({
      st <- state$settings
      keys <- c("align", "filter", "distance", "tree", "bootstrap", "k", "groups")
      out <- stats::setNames(vector("list", length(keys)), keys)
      for (k in keys) if (!is.null(st[[k]])) out[k] <- list(st[[k]])
      # Keep alignment only for sequences that were not aligned on arrival
      if (!(state$raw$type %in% c("dna", "protein") && !isTRUE(state$raw$aligned))) out["align"] <- list(NULL)
      out
    })

    source_info <- shiny::reactive({
      s <- state$raw$source
      if (!is.null(s$demo)) return(list(demo = s$demo))
      list(file = s$file, type = state$raw$type, transposed = s$transposed)
    })

    script_text <- shiny::reactive({
      shiny::req(state$raw)
      build_script(settings(), source_info(), state$meta_file)
    })
    output$script_text <- shiny::renderText(script_text())

    output$checklist <- shiny::renderUI({
      item <- function(ok, text) {
        shiny::tags$li(class = if (ok) "is-in" else "is-out",
                       bsicons::bs_icon(if (ok) "check-circle-fill" else "circle"), shiny::span(text))
      }
      shiny::tags$ul(
        class = "cl-checklist",
        item(TRUE, "Data summary"),
        item(!is.null(state$data$filter), "Quality filters"),
        item(!is.null(state$dist), "Distance measure and heatmap"),
        item(!is.null(state$tree), "Tree and cophenetic correlation"),
        item(!is.null(state$boot), "Bootstrap support"),
        item(!is.null(state$kres), "Number of groups"),
        item(!is.null(state$groups), "Groups, PCoA or PCA, and membership table"),
        item(!is.null(state$mantel), "Mantel test"),
        item(TRUE, "Settings and software versions")
      )
    })

    output$report <- shiny::downloadHandler(
      filename = function() paste0("clusnalyst_report_", format(Sys.Date()), ".html"),
      content = function(file) {
        shiny::withProgress(message = "Writing the report", value = 0.5, {
          res <- list(raw = state$raw, data = state$data, dist = state$dist, tree = state$tree,
                      boot = state$boot, kres = state$kres, groups = state$groups, ord = state$ord,
                      mantel = state$mantel)
          build_report(res, settings(), file, title = input$title %||% "Clusnalyst report",
                       tree_layout = input$layout %||% "rectangular")
        })
      }
    )
    output$script <- shiny::downloadHandler(
      filename = function() "clusnalyst_analysis.R",
      content = function(file) writeLines(script_text(), file)
    )
    output$session <- shiny::downloadHandler(
      filename = function() paste0("clusnalyst_session_", format(Sys.Date()), ".json"),
      content = function(file) save_session(state$raw, settings(), file)
    )
  })
}
