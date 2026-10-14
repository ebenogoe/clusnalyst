#' Clusnalyst's own theme: violet with coral and mint accents, Plus Jakarta Sans
#' @noRd
cl_theme <- function() {
  # Loaded from Google Fonts by the visitor's browser; offline it falls back to system fonts
  sans <- bslib::font_collection(bslib::font_google("Plus Jakarta Sans", wght = "400..800", local = FALSE),
                                 "system-ui", "-apple-system", "Segoe UI", "sans-serif")
  mono <- bslib::font_collection("ui-monospace", "SF Mono", "Menlo", "Consolas", "monospace")
  bslib::bs_theme(
    version = 5,
    primary = "#5b3fd6", secondary = "#5e5a78", success = "#12806a",
    info = "#2563c9", warning = "#9a4a00", danger = "#c0362c",
    base_font = sans, heading_font = sans, code_font = mono,
    "body-bg" = "#ffffff", "body-color" = "#2d2a45",
    "body-bg-dark" = "#121022", "body-color-dark" = "#e2dff2",
    "link-color" = "#5136c9", "link-color-dark" = "#b3a6ff",
    "headings-font-weight" = 800,
    "border-radius" = "10px", "border-radius-sm" = "8px", "border-radius-lg" = "14px",
    "font-size-base" = "1rem",
    "navbar-light-brand-color" = "#1e1b33"
  )
}

steps_def <- c(data = "Data", qc = "Quality check", distance = "Distances", tree = "Tree",
               validate = "Validate", groups = "Groups", report = "Report")

#' Heading block at the top of each analysis step
#' @noRd
step_header <- function(n, title, text, ...) {
  shiny::div(
    class = "cl-step-head",
    shiny::div(
      shiny::span(class = "cl-kicker", sprintf("Step %d of %d", n, length(steps_def))),
      shiny::h2(title),
      shiny::p(class = "cl-lede", text)
    ),
    shiny::div(class = "cl-step-head-actions", ...)
  )
}

#' Small information button that opens a popover
#' @noRd
info <- function(..., title = NULL) {
  bslib::popover(
    shiny::tags$button(type = "button", class = "cl-info", `aria-label` = "More information",
                       bsicons::bs_icon("info-circle")),
    ..., title = title, options = list(trigger = "focus click")
  )
}

#' Input label with an information popover
#' @noRd
label_info <- function(label, ..., title = NULL) {
  shiny::tags$span(class = "cl-label", label, info(..., title = title))
}

#' Number tiles, styled after the site's project highlights
#' @noRd
stat_tile <- function(value, label, note = NULL) {
  shiny::div(
    class = "cl-stat",
    shiny::div(class = "cl-stat-value", value),
    shiny::div(class = "cl-stat-label", label),
    if (!is.null(note)) shiny::div(class = "cl-stat-note", note)
  )
}

stat_row <- function(...) shiny::div(class = "cl-stats", ...)

#' Message shown where results will appear
#' @noRd
empty_state <- function(text, ...) {
  shiny::div(class = "cl-empty", bsicons::bs_icon("diagram-3", size = "1.6rem"),
             shiny::p(text), ...)
}

#' Download menu for a plot (PNG, SVG, PDF)
#' @noRd
download_menu <- function(ns, id, extra = NULL) {
  item <- function(suffix, label) {
    shiny::tags$li(shiny::downloadLink(ns(paste0(id, "_", suffix)), label, class = "dropdown-item"))
  }
  shiny::div(
    class = "dropdown cl-download",
    shiny::tags$button(
      type = "button", class = "btn btn-sm btn-outline-secondary dropdown-toggle",
      `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
      bsicons::bs_icon("download"), shiny::span("Download")
    ),
    shiny::tags$ul(class = "dropdown-menu dropdown-menu-end",
                   item("png", "PNG image (300 dpi)"), item("svg", "SVG (editable)"),
                   item("pdf", "PDF"), extra)
  )
}

#' Card holding an interactive plot
#' @noRd
plot_card <- function(ns, id, title, height = NULL, extra_menu = NULL, footer = NULL,
                      interactive = TRUE) {
  bslib::card(
    full_screen = TRUE,
    class = "cl-plot-card",
    bslib::card_header(
      class = "cl-card-head",
      shiny::span(title),
      download_menu(ns, id, extra_menu)
    ),
    bslib::card_body(
      if (interactive) ggiraph::girafeOutput(ns(id), height = height)
      else shiny::plotOutput(ns(paste0(id, "_static")), height = height %||% "520px")
    ),
    if (!is.null(footer)) bslib::card_footer(footer)
  )
}

#' Register PNG, SVG and PDF downloads for a plot
#' @noRd
plot_downloads <- function(output, id, plot_fn, name, width = 8, height = 6) {
  wh <- function(v) if (is.function(v)) v() else v
  save <- function(file, device) {
    p <- plot_fn()
    ggplot2::ggsave(file, p, width = wh(width), height = wh(height), dpi = 300,
                    bg = "white", device = device, limitsize = FALSE)
  }
  output[[paste0(id, "_png")]] <- shiny::downloadHandler(
    filename = function() paste0(name(), ".png"),
    content = function(file) save(file, "png")
  )
  output[[paste0(id, "_svg")]] <- shiny::downloadHandler(
    filename = function() paste0(name(), ".svg"),
    content = function(file) save(file, svglite::svglite)
  )
  output[[paste0(id, "_pdf")]] <- shiny::downloadHandler(
    filename = function() paste0(name(), ".pdf"),
    content = function(file) save(file, grDevices::pdf)
  )
  keep_downloads(output, paste0(id, c("_png", "_svg", "_pdf")))
}

#' Download links inside a closed dropdown count as hidden, and Shiny does not
#' give hidden download links a target. Keep them active.
#' @noRd
keep_downloads <- function(output, ids) {
  for (i in ids) shiny::outputOptions(output, i, suspendWhenHidden = FALSE)
}

#' Render a ggplot as an interactive SVG with the app's settings
#' @noRd
cl_girafe <- function(p, width = 8, height = 6, dark = FALSE) {
  tip_css <- if (dark) {
    "background:#f4f2ff;color:#121022;padding:7px 11px;border-radius:10px;font-family:'Plus Jakarta Sans',sans-serif;font-size:13px;max-width:280px;"
  } else {
    "background:#1e1b33;color:#fff;padding:7px 11px;border-radius:10px;font-family:'Plus Jakarta Sans',sans-serif;font-size:13px;max-width:280px;"
  }
  ggiraph::girafe(
    ggobj = p, width_svg = width, height_svg = height, bg = "transparent",
    options = list(
      ggiraph::opts_sizing(rescale = TRUE),
      ggiraph::opts_tooltip(css = tip_css, offx = 14, offy = 14, use_fill = FALSE),
      ggiraph::opts_hover(css = "stroke:#D55E00;stroke-width:2px;"),
      ggiraph::opts_hover_inv(css = "opacity:0.55;"),
      ggiraph::opts_selection(type = "none"),
      ggiraph::opts_zoom(min = 0.5, max = 6),
      ggiraph::opts_toolbar(position = "topright", saveaspng = FALSE,
                            hidden = c("lasso_select", "lasso_deselect"))
    )
  )
}

#' Run code and turn errors into a plain notification
#' @noRd
safely <- function(expr, what = "that step") {
  tryCatch(
    withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning")),
    error = function(e) {
      shiny::showNotification(
        shiny::tags$span(shiny::tags$b(sprintf("Couldn't finish %s.", what)), shiny::tags$br(),
                         conditionMessage(e)),
        type = "error", duration = 15
      )
      NULL
    }
  )
}

#' Button leading to the next step
#' @noRd
next_button <- function(id, label) {
  shiny::actionButton(id, shiny::tagList(label, bsicons::bs_icon("arrow-right")),
                      class = "btn-primary cl-next")
}

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

fmt_pct <- function(x, digits = 1) {
  if (is.null(x) || is.na(x)) return("n/a")
  paste0(formatC(100 * x, format = "f", digits = digits), "%")
}

#' Table of a data frame with the app's defaults
#' @noRd
cl_reactable <- function(df, ..., page_size = 15, digits = 3) {
  rownames(df) <- NULL
  # Whole-number columns (counts, cluster numbers) are shown without decimals
  num <- vapply(df, function(v) is.numeric(v) && any(abs(v - round(v)) > 1e-9, na.rm = TRUE), logical(1))
  cols <- lapply(names(df)[num], function(n) reactable::colDef(format = reactable::colFormat(digits = digits)))
  names(cols) <- names(df)[num]
  reactable::reactable(
    df, ..., columns = cols, defaultPageSize = page_size, compact = TRUE, striped = FALSE,
    highlight = TRUE, searchable = nrow(df) > 15, resizable = TRUE, wrap = FALSE,
    defaultColDef = reactable::colDef(minWidth = 90, headerClass = "cl-th"),
    class = "cl-table",
    language = reactable::reactableLang(searchPlaceholder = "Search")
  )
}

#' Group choices for colouring plots, from metadata
#' @noRd
meta_group_choices <- function(meta) {
  if (is.null(meta)) return(character())
  ok <- vapply(meta, function(v) {
    u <- length(unique(stats::na.omit(v)))
    u >= 2 && u <= 20 && (is.character(v) || is.factor(v) || u <= 8)
  }, logical(1))
  nm <- names(meta)[ok]
  stats::setNames(paste0("meta:", nm), paste("Metadata:", nm))
}

#' Hover text for each sample
#' @noRd
sample_tooltips <- function(ids, meta = NULL, groups = NULL) {
  out <- ids
  if (!is.null(groups)) out <- paste0(out, "\nCluster ", groups[ids])
  if (!is.null(meta)) {
    m <- meta[ids, , drop = FALSE]
    for (n in utils::head(names(m), 6)) {
      v <- m[[n]]
      v <- if (is.numeric(v)) format(v, digits = 4) else as.character(v)
      out <- paste0(out, "\n", n, ": ", v)
    }
  }
  stats::setNames(gsub("\n", "<br>", htmltools::htmlEscape(out)), ids)
}
