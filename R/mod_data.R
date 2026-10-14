mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  demos <- demo_info()
  shiny::tagList(
    step_header(1, "Load data",
                "Upload a table, VCF, HapMap or FASTA file, load a demo dataset, or resume a saved session."),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
      shiny::div(
        bslib::card(
          bslib::card_header("Data source"),
          bslib::navset_underline(
            id = ns("src"),
            bslib::nav_panel(
              "Upload", value = "upload",
              shiny::div(
                class = "pt-3",
                shiny::fileInput(
                  ns("file"),
                  label_info("Data file",
                             shiny::p("Tables: one row per sample, sample names in the first column. One row per marker also works; the app spots it."),
                             shiny::p("Markers: 0/1 for bands; AA/AG/GG, A/G, IUPAC letters or 0/1/2 for SNPs. Blank, NA or NN means missing."),
                             shiny::p("Also VCF (.vcf, .vcf.gz), HapMap (.hmp.txt) and FASTA."),
                             title = "Accepted data formats"),
                  accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".vcf", ".gz", ".fasta",
                             ".fa", ".fas", ".fna", ".faa", ".aln"),
                  placeholder = "No file chosen", width = "100%"
                ),
                bslib::accordion(
                  open = FALSE,
                  bslib::accordion_panel(
                    "Reading options",
                    icon = bsicons::bs_icon("sliders"),
                    shiny::selectInput(ns("type"), "Data type",
                                       c("Detect automatically" = "auto", data_types())),
                    shiny::radioButtons(ns("orient"), "Table layout",
                                        c("Detect automatically" = "auto",
                                          "One row per sample" = "rows",
                                          "One row per marker" = "cols")),
                    shiny::actionButton(ns("reread"), "Read the file again", class = "btn-outline-primary btn-sm")
                  )
                )
              )
            ),
            bslib::nav_panel(
              "Demo data", value = "demo",
              shiny::div(
                class = "pt-3",
                shiny::radioButtons(
                  ns("demo"), NULL,
                  choiceNames = lapply(seq_len(nrow(demos)), function(i) {
                    shiny::span(class = "cl-choice", shiny::tags$b(demos$title[i]),
                                shiny::span(demos$description[i]))
                  }),
                  choiceValues = demos$name, width = "100%"
                ),
                shiny::actionButton(ns("load_demo"), shiny::tagList(bsicons::bs_icon("play-circle"), "Load this demo"),
                                    class = "btn-primary")
              )
            ),
            bslib::nav_panel(
              "Resume", value = "resume",
              shiny::div(
                class = "pt-3",
                shiny::p("Open a session file exported from the Export step. All steps are rerun with the saved settings and seeds."),
                shiny::fileInput(ns("session"), "Session file (.json)", accept = ".json", width = "100%")
              )
            )
          )
        ),
        bslib::card(
          bslib::card_header(label_info("Sample information (optional)",
            "A table with sample names in the first column and anything else in the other columns: population, origin, latitude and longitude, and so on. These columns can colour the tree and the PCoA, appear when you hover over a sample, and feed a Mantel test.")),
          shiny::fileInput(ns("meta"), NULL, accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls"),
                           placeholder = "No file chosen", width = "100%"),
          shiny::uiOutput(ns("meta_status"))
        )
      ),
      shiny::div(
        shiny::div(id = ns("summary"), shiny::uiOutput(ns("summary_ui"))),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Preview"),
          shiny::uiOutput(ns("preview_ui"))
        )
      )
    )
  )
}

mod_data_server <- function(id, state, goto, lim, dark) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    upload <- shiny::reactiveVal(NULL)

    set_data <- function(d, meta_file = NULL) {
      if (d$type %in% c("dna", "protein") && length(d$seqs) > lim$max_sequences) {
        shiny::showNotification(sprintf("This copy of the app handles up to %d sequences. Your file has %d. Install the package to run it locally.",
                                        lim$max_sequences, length(d$seqs)), type = "error", duration = 15)
        return(invisible(FALSE))
      }
      reset_after(state, "data")
      state$aligned <- NULL
      state$raw <- d
      state$settings <- list()
      state$meta_file <- meta_file
      state$loaded_at <- state$loaded_at + 1
      invisible(TRUE)
    }

    read_upload <- function() {
      f <- upload()
      shiny::req(f)
      type <- if (identical(input$type, "auto")) NULL else input$type
      tr <- switch(input$orient, auto = NULL, rows = FALSE, cols = TRUE)
      d <- safely(read_markers(f$datapath, name = f$name, type = type, transpose = tr), "reading the file")
      if (!is.null(d)) {
        if (set_data(d)) {
          shiny::updateSelectInput(session, "type", selected = d$type)
          if (!is.null(d$source$transposed)) {
            shiny::updateRadioButtons(session, "orient", selected = if (isTRUE(d$source$transposed)) "cols" else "rows")
          }
        }
      }
    }

    shiny::observeEvent(input$file, {
      upload(input$file)
      shiny::updateSelectInput(session, "type", selected = "auto")
      shiny::updateRadioButtons(session, "orient", selected = "auto")
      shiny::isolate(read_upload())
    })
    shiny::observeEvent(input$reread, read_upload())

    shiny::observeEvent(input$load_demo, {
      d <- safely(clusnalyst_demo(input$demo), "loading the demo")
      if (!is.null(d)) set_data(d)
    })

    shiny::observeEvent(input$meta, {
      shiny::req(state$raw)
      m <- safely(read_metadata(input$meta$datapath, input$meta$name), "reading the sample information")
      if (is.null(m)) return()
      d <- safely(attach_metadata(state$raw, m), "matching the sample information")
      if (is.null(d)) return()
      state$raw <- d
      state$meta_file <- input$meta$name
      for (f in c("aligned", "data")) {
        if (!is.null(state[[f]])) {
          x <- state[[f]]
          x$meta <- d$meta[sample_names(x), , drop = FALSE]
          state[[f]] <- x
        }
      }
    })

    shiny::observeEvent(input$session, {
      s <- safely(load_session(input$session$datapath), "opening the session file")
      if (is.null(s)) return()
      d <- s$data
      if (!set_data(d)) return()
      st <- s$settings
      res <- shiny::withProgress(message = "Rebuilding the session", value = 0, {
        steps <- c("Aligning sequences", "Filtering", "Computing distances", "Building the tree",
                   "Bootstrapping", "Comparing numbers of groups", "Finding groups")
        safely(run_analysis(d, st, progress = function(msg) {
          shiny::incProgress(1 / length(steps), detail = msg)
        }), "rebuilding the session")
      })
      if (is.null(res)) return()
      if (!identical(d$aligned, TRUE) && !is.null(res$data$x) && d$type %in% c("dna", "protein")) {
        state$aligned <- res$data
      }
      state$data <- res$data
      state$dist <- res$dist
      state$tree <- res$tree
      state$boot <- res$boot
      state$kres <- res$kres
      state$groups <- res$groups
      state$ord <- res$ord
      state$settings <- st
      shiny::showNotification("Session restored. Every step has been run again.", type = "message")
      goto(if (!is.null(res$tree)) "tree" else if (!is.null(res$dist)) "distance" else "qc")
    })

    output$meta_status <- shiny::renderUI({
      d <- state$raw
      if (is.null(d)) return(shiny::p(class = "cl-muted small", "Load data first."))
      if (is.null(d$meta)) return(shiny::p(class = "cl-muted small", "None loaded."))
      hit <- attr(d$meta, "matched") %||% nrow(d$meta)
      shiny::p(class = "small",
               bsicons::bs_icon("check-circle", class = "text-success"),
               sprintf(" %d columns (%s) for %d of %d samples.", ncol(d$meta),
                       paste(names(d$meta), collapse = ", "), hit, n_samples(d)))
    })

    output$summary_ui <- shiny::renderUI({
      d <- state$raw
      if (is.null(d)) {
        return(bslib::card(empty_state("Load a file or a demo dataset. View summary here.")))
      }
      q <- qc_summary(d)
      cols <- if (!is.null(d$x)) ncol(d$x) else NA
      src <- d$source
      how <- c(
        if (!is.null(src$format)) sprintf("Read as %s", toupper(src$format)),
        if (!is.null(src$encoding)) switch(src$encoding, letters = "letter genotypes converted to 0/1/2",
                                           dosage = "0/1/2 dosages", binary = "0/1 scores",
                                           numeric = "numeric traits", mixed = "numeric and categorical traits"),
        if (isTRUE(src$transposed)) "markers were in rows, so the table was turned round",
        if (d$type %in% c("dna", "protein")) if (isTRUE(d$aligned)) "already aligned" else "not aligned yet"
      )
      bslib::card(
        class = "cl-summary-card",
        bslib::card_header(class = "cl-card-head", shiny::span(src$file %||% "Data"),
                           shiny::span(class = "badge text-bg-light", type_label(d$type))),
        stat_row(
          stat_tile(fmt_n(n_samples(d)), "samples"),
          if (!is.na(cols)) stat_tile(fmt_n(cols), unit_label(d$type, cols)),
          if (!is.null(q$missing) && !is.na(q$missing)) stat_tile(fmt_pct(q$missing), if (d$type %in% c("dna", "protein")) "gaps" else "missing"),
          if (d$type %in% c("binary", "snp")) stat_tile(fmt_n(q$n_monomorphic), "not polymorphic"),
          if (d$type == "quant") stat_tile(fmt_n(q$n_categorical), "categorical traits")
        ),
        shiny::p(class = "cl-muted small mb-2", paste0(paste(how, collapse = "; "), ".")),
        shiny::div(
          class = "cl-row-end",
          shiny::span(class = "cl-muted small", "To correct the data type or layout, use Reading options."),
          next_button(ns("next"), "Quality control")
        )
      )
    })

    output$preview_ui <- shiny::renderUI({
      d <- state$raw
      if (is.null(d)) return(shiny::p(class = "cl-muted p-3", "Nothing loaded yet."))
      reactable::reactableOutput(ns("preview"))
    })

    output$preview <- reactable::renderReactable({
      d <- state$raw
      shiny::req(d)
      if (d$type %in% c("dna", "protein")) {
        df <- data.frame(sample = names(d$seqs), length = nchar(gsub("-", "", d$seqs)),
                         start = substr(d$seqs, 1, 60))
      } else {
        x <- d$x
        keep <- seq_len(min(ncol(x), 40))
        df <- data.frame(sample = rownames(x), as.data.frame(x[, keep, drop = FALSE], check.names = FALSE),
                         check.names = FALSE)
        if (!is.null(d$meta)) df <- cbind(df[1], d$meta[df$sample, , drop = FALSE], df[-1])
      }
      cl_reactable(df, page_size = 12, digits = 3)
    })

    shiny::observeEvent(input[["next"]], goto("qc"))
  })
}
