mod_qc_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    step_header(2, "Quality control",
                "Review missing data, allele frequencies and heterozygosity, and set filtering thresholds. Data are removed only when filters are applied."),
    shiny::uiOutput(ns("body"))
  )
}

mod_qc_server <- function(id, state, goto, lim, dark) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    is_seq <- shiny::reactive(!is.null(state$raw) && state$raw$type %in% c("dna", "protein"))

    # What the filters run on: the aligned copy for sequences, else the raw data
    base <- shiny::reactive({
      d <- state$raw
      shiny::req(d)
      if (is_seq()) {
        if (!is.null(state$aligned)) state$aligned else if (isTRUE(d$aligned)) d else NULL
      } else {
        d
      }
    })

    qc <- shiny::reactive({
      b <- if (is_seq()) base() %||% state$raw else base()
      qc_summary(b)
    })

    output$body <- shiny::renderUI({
      d <- state$raw
      if (is.null(d)) return(bslib::card(empty_state("Load data in step 1 first.",
                                                     shiny::actionButton(ns("back"), "Go to step 1", class = "btn-outline-primary"))))
      defaults <- default_settings(d)$filter
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8)),
        shiny::div(
          if (is_seq()) align_card(ns, d, state$aligned),
          bslib::card(
            bslib::card_header("Filters"),
            if (d$type %in% c("binary", "snp")) shiny::tagList(
              shiny::sliderInput(ns("max_marker_missing"),
                                 label_info("Maximum missing rate per marker",
                                            "Markers missing in a larger proportion of samples are removed. 0.2 is a common threshold."),
                                 0, 1, defaults$max_marker_missing, step = 0.05, width = "100%"),
              shiny::sliderInput(ns("min_maf"),
                                 label_info(if (d$type == "snp") "Minimum minor allele frequency" else "Minimum minor band frequency",
                                            "Markers with a rarer allele (or band state) below this frequency are removed. Rare variants carry little information on group structure. 0.05 is standard for SNPs."),
                                 0, 0.5, defaults$min_maf, step = 0.01, width = "100%"),
              shiny::checkboxInput(ns("drop_mono"), "Remove monomorphic markers", TRUE)
            ),
            if (d$type == "quant") shiny::tagList(
              shiny::sliderInput(ns("max_marker_missing"), "Maximum missing rate per trait", 0, 1, 0.2, step = 0.05, width = "100%"),
              shiny::checkboxInput(ns("drop_mono"), "Remove constant traits", TRUE)
            ),
            shiny::sliderInput(ns("max_sample_missing"),
                               label_info(if (is_seq()) "Maximum gap proportion per sequence" else "Maximum missing rate per sample",
                                          "Applied after marker filtering. Samples above this threshold are removed."),
                               0, 1, defaults$max_sample_missing, step = 0.05, width = "100%"),
            if (is_seq()) shiny::checkboxInput(ns("drop_empty"), "Remove all-gap alignment columns", TRUE),
            shiny::div(
              class = "cl-row-end",
              shiny::actionLink(ns("skip"), "Skip filtering"),
              shiny::actionButton(ns("apply"), "Apply filters", class = "btn-primary")
            )
          )
        ),
        shiny::div(
          shiny::uiOutput(ns("result")),
          if (is_seq()) bslib::card(full_screen = TRUE, bslib::card_header("Alignment preview"),
                                    shiny::uiOutput(ns("aln_preview"))),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(class = "cl-card-head", shiny::span("Distributions"),
                               shiny::span(class = "cl-muted small", "Dashed lines mark current thresholds")),
            shiny::plotOutput(ns("qc_plot"), height = "420px")
          ),
          bslib::navset_card_underline(
            full_screen = TRUE,
            bslib::nav_panel("Samples", reactable::reactableOutput(ns("samples"))),
            if (!is_seq()) bslib::nav_panel(if (d$type == "quant") "Traits" else "Markers",
                                            reactable::reactableOutput(ns("markers")))
          )
        )
      )
    })

    shiny::observeEvent(input$back, goto("data"))

    shiny::observeEvent(input$align, {
      d <- state$raw
      shiny::req(d)
      if (length(d$seqs) > lim$max_sequences) {
        shiny::showNotification(sprintf("Up to %d sequences can be aligned here.", lim$max_sequences), type = "error")
        return()
      }
      a <- shiny::withProgress(message = sprintf("Aligning %d sequences with %s", length(d$seqs), input$aln_method), value = 0.3, {
        safely(align_sequences(d, input$aln_method), "the alignment")
      })
      if (is.null(a)) return()
      reset_after(state, "qc")
      state$data <- NULL
      state$aligned <- a
      set_setting(state, "align", list(method = input$aln_method))
    })

    thresholds <- shiny::reactive(list(
      marker_missing = input$max_marker_missing, maf = input$min_maf,
      sample_missing = input$max_sample_missing
    ))

    output$qc_plot <- shiny::renderPlot({
      shiny::req(state$raw)
      plot_qc(qc(), dark = dark(), thresholds = thresholds())
    }, bg = "transparent", res = 96)

    # Live count of what the filters would keep
    preview_counts <- shiny::reactive({
      b <- base()
      shiny::req(b)
      f <- tryCatch(filter_data(b, input$max_marker_missing %||% 1, input$min_maf %||% 0,
                                isTRUE(input$drop_mono), input$max_sample_missing %||% 1,
                                isTRUE(input$drop_empty %||% TRUE)),
                    error = function(e) e)
      f
    })

    output$result <- shiny::renderUI({
      d <- state$raw
      shiny::req(d)
      b <- base()
      if (is.null(b)) {
        return(bslib::card(empty_state("Align the sequences first. Filtering and distances work on the aligned sites.")))
      }
      f <- preview_counts()
      unit <- unit_label(d$type)
      if (inherits(f, "error")) {
        body <- shiny::div(class = "alert alert-warning mb-0", conditionMessage(f))
      } else {
        body <- stat_row(
          stat_tile(sprintf("%s / %s", fmt_n(f$filter$after[["samples"]]), fmt_n(f$filter$before[["samples"]])), "samples kept"),
          stat_tile(sprintf("%s / %s", fmt_n(f$filter$after[["columns"]]), fmt_n(f$filter$before[["columns"]])), paste(unit, "kept")),
          if (d$type == "snp") stat_tile(fmt_pct(mean(qc()$markers$heterozygosity, na.rm = TRUE)), "mean heterozygosity")
        )
      }
      applied <- state$data
      bslib::card(
        bslib::card_header(class = "cl-card-head",
                           shiny::span("Filter preview"),
                           if (!is.null(applied)) shiny::span(class = "badge text-bg-success", "Applied")),
        body,
        if (!is.null(applied)) shiny::div(
          class = "cl-row-end",
          shiny::span(class = "cl-muted small",
                      if (is.null(applied$filter)) "No filters applied."
                      else sprintf("Using %s samples and %s %s.", fmt_n(nrow(applied$x)), fmt_n(ncol(applied$x)), unit)),
          next_button(ns("next"), "Distance matrix")
        )
      )
    })

    shiny::observeEvent(input$apply, {
      b <- base()
      if (is.null(b)) {
        shiny::showNotification("Align the sequences first.", type = "warning")
        return()
      }
      f <- safely(filter_data(b, input$max_marker_missing %||% 1, input$min_maf %||% 0,
                              isTRUE(input$drop_mono), input$max_sample_missing %||% 1,
                              isTRUE(input$drop_empty %||% TRUE)), "filtering")
      if (is.null(f)) return()
      reset_after(state, "qc")
      if (!is.null(state$aligned)) set_setting(state, "align", list(method = state$aligned$source$alignment))
      state$data <- f
      set_setting(state, "filter", f$filter$settings)
      shiny::showNotification(sprintf("Filters applied: %s samples and %s %s kept.",
                                      fmt_n(nrow(f$x)), fmt_n(ncol(f$x)), unit_label(f$type)), type = "message")
    })

    shiny::observeEvent(input$skip, {
      b <- base()
      if (is.null(b)) {
        shiny::showNotification("Align the sequences first.", type = "warning")
        return()
      }
      reset_after(state, "qc")
      if (!is.null(state$aligned)) set_setting(state, "align", list(method = state$aligned$source$alignment))
      state$data <- b
      set_setting(state, "filter", NULL)
      goto("distance")
    })

    shiny::observeEvent(input[["next"]], goto("distance"))

    output$samples <- reactable::renderReactable({
      s <- qc()$samples
      s <- s[, colSums(!is.na(s)) > 0, drop = FALSE]
      if (!is.null(state$raw$meta)) s <- cbind(s, state$raw$meta[s$sample, , drop = FALSE])
      cl_reactable(s, digits = 3,
                   rowStyle = function(i) {
                     if (!is.null(s$missing) && isTRUE(s$missing[i] > (input$max_sample_missing %||% 1))) {
                       list(background = "rgba(213, 94, 0, 0.12)")
                     }
                   })
    })

    output$markers <- reactable::renderReactable({
      m <- qc()$markers
      m <- m[, colSums(!is.na(m)) > 0, drop = FALSE]
      if (nrow(m) > 5000) m <- m[seq_len(5000), ]
      cl_reactable(m, digits = 3)
    })

    output$aln_preview <- shiny::renderUI({
      b <- base()
      if (is.null(b)) return(shiny::p(class = "cl-muted p-2", "No alignment yet."))
      alignment_html(b$x, b$type)
    })
  })
}

align_card <- function(ns, d, aligned) {
  done <- isTRUE(d$aligned) || !is.null(aligned)
  bslib::card(
    bslib::card_header(class = "cl-card-head", shiny::span("Alignment"),
                       if (done) shiny::span(class = "badge text-bg-success", "Aligned")),
    if (isTRUE(d$aligned)) {
      shiny::p("These sequences all have the same length, so they are treated as aligned.")
    } else {
      shiny::tagList(
        if (done) shiny::p(sprintf("Aligned with %s: %d sites. Pick another method and align again if you want to compare.",
                                   aligned$source$alignment, ncol(aligned$x)))
        else shiny::p(sprintf("%d sequences of different lengths. Align them before filtering.", length(d$seqs))),
        shiny::selectInput(ns("aln_method"),
                           label_info("Method",
                                      "ClustalOmega is fast and accurate for most sets. ClustalW is the classic choice. MUSCLE does well on protein families. All three run through the msa package."),
                           c("ClustalOmega", "ClustalW", "Muscle")),
        shiny::actionButton(ns("align"), if (done) "Align again" else "Align sequences",
                            class = if (done) "btn-outline-primary" else "btn-primary")
      )
    }
  )
}

# Colour-coded view of the first part of an alignment
alignment_html <- function(x, type = "dna", max_rows = 40, max_cols = 120) {
  x <- x[seq_len(min(nrow(x), max_rows)), seq_len(min(ncol(x), max_cols)), drop = FALSE]
  rows <- lapply(seq_len(nrow(x)), function(i) {
    chars <- x[i, ]
    cls <- ifelse(chars %in% c("-", "?", "."), "gap", tolower(chars))
    spans <- paste0("<span class='r-", cls, "'>", chars, "</span>", collapse = "")
    paste0("<div class='cl-aln-row'><span class='cl-aln-name'>", htmltools::htmlEscape(rownames(x)[i]),
           "</span><span class='cl-aln-seq'>", spans, "</span></div>")
  })
  shiny::div(
    class = paste0("cl-aln is-", type),
    shiny::HTML(paste(unlist(rows), collapse = "")),
    shiny::p(class = "cl-muted small mt-2",
             sprintf("Showing the first %d sites of %d sequences.", ncol(x), nrow(x)))
  )
}
