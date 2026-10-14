mod_home_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "cl-home",
    shiny::tags$section(
      class = "cl-hero",
      bubbles_svg(),
      shiny::div(
        class = "cl-hero-text",
        shiny::p(class = "cl-kicker", "Cluster analysis without code"),
        shiny::h1("Clus", shiny::span("nalyst", .noWS = "outside")),
        shiny::p(class = "cl-hero-lede",
                 "Group samples by how alike they are, from marker, trait or sequence data. Then see how many groups your data supports and how far to trust each one."),
        shiny::div(
          class = "cl-hero-actions",
          shiny::actionButton(ns("go_upload"), shiny::tagList(bsicons::bs_icon("upload"), "Upload your data"),
                              class = "btn-primary btn-lg"),
          shiny::actionButton(ns("go_demo"), shiny::tagList(bsicons::bs_icon("play-circle"), "Try a demo dataset"),
                              class = "btn-outline-primary btn-lg")
        ),
        shiny::p(class = "cl-hero-small",
                 "Nothing is stored after you close the page. Save a session file if you want to come back to your work.")
      ),
      shiny::div(
        class = "cl-hero-figure",
        bslib::card(
          class = "cl-hero-card",
          bslib::card_header(class = "cl-card-head",
                             shiny::span("SNP demo, clustered by UPGMA"),
                             shiny::span(class = "cl-muted small", "Hover over a sample for details")),
          bslib::card_body(ggiraph::girafeOutput(ns("preview"), height = NULL))
        )
      )
    )
  )
}

flow_item <- function(title, text) {
  shiny::tags$li(shiny::tags$b(title), shiny::span(text))
}

format_row <- function(a, b, c) {
  shiny::tags$tr(shiny::tags$td(a), shiny::tags$td(b), shiny::tags$td(c))
}

mod_home_server <- function(id, goto, dark) {
  shiny::moduleServer(id, function(input, output, session) {
    demo <- shiny::reactive({
      d <- filter_data(clusnalyst_demo("snp"), min_maf = 0.05)
      dm <- compute_distance(d)
      list(d = d, tree = build_tree(dm, "upgma"))
    })
    output$preview <- ggiraph::renderGirafe({
      x <- demo()
      g <- stats::setNames(x$d$meta$population, rownames(x$d$meta))
      p <- plot_tree(x$tree, layout = "circular", groups = g, group_title = "Population",
                     tooltip = sample_tooltips(names(g), x$d$meta), label_size = 2.9,
                     dark = dark(), show_scale = FALSE)
      # A preview only: hover for details, no zoom or toolbar
      cl_girafe(p, 6, 6, dark = dark(), toolbar = FALSE)
    })
    shiny::observeEvent(input$go_upload, {
      goto("data")
      bslib::nav_select("data-src", "upload", session = session$rootScope())
    })
    shiny::observeEvent(input$go_demo, {
      goto("data")
      bslib::nav_select("data-src", "demo", session = session$rootScope())
    })
  })
}

# Decorative cluster bubbles behind the hero: three loose groups in the brand colours
bubbles_svg <- function() {
  set <- function(cx, cy, col, pts) {
    paste0(vapply(pts, function(p) sprintf('<circle cx="%s" cy="%s" r="%s" fill="%s" opacity="%s"/>',
                                           cx + p[1], cy + p[2], p[3], col, p[4]), character(1)), collapse = "")
  }
  a <- list(c(0, 0, 46, .16), c(70, -30, 26, .22), c(-60, 50, 30, .18), c(40, 70, 18, .3), c(-20, -70, 14, .3))
  b <- list(c(0, 0, 38, .18), c(-55, -20, 22, .26), c(45, 40, 28, .2), c(10, -60, 12, .34))
  c <- list(c(0, 0, 34, .2), c(50, -25, 20, .28), c(-35, 45, 16, .32), c(30, 55, 10, .4))
  svg <- paste0('<svg class="cl-bubbles" viewBox="0 0 700 520" aria-hidden="true" focusable="false">',
                set(470, 140, "#5b3fd6", a), set(240, 330, "#ff7a59", b), set(560, 390, "#2bb59a", c),
                '</svg>')
  shiny::HTML(svg)
}
