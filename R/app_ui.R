#' The application user interface
#' @param request Internal parameter for `{shiny}`.
#' @noRd
app_ui <- function(request) {
  lim <- app_limits()
  shiny::tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      id = "nav",
      title = shiny::tags$span(
        class = "cl-brand",
        shiny::tags$img(src = "www/logo.svg", alt = "", width = 28, height = 28),
        shiny::tags$span("Clusnalyst")
      ),
      window_title = "Clusnalyst: cluster analysis without code",
      theme = cl_theme(),
      fillable = FALSE,
      lang = "en",
      navbar_options = bslib::navbar_options(collapsible = TRUE, underline = TRUE),
      header = shiny::useBusyIndicators(spinners = TRUE, pulse = TRUE),
      bslib::nav_panel("Home", value = "home", icon = bsicons::bs_icon("house"),
                       mod_home_ui("home")),
      bslib::nav_panel("Analyse", value = "analyse", icon = bsicons::bs_icon("diagram-3"),
                       analyse_ui(lim)),
      bslib::nav_panel("Guide", value = "guide", icon = bsicons::bs_icon("book"),
                       mod_guide_ui("guide")),
      bslib::nav_panel("About", value = "about", icon = bsicons::bs_icon("person"),
                       mod_about_ui("about")),
      bslib::nav_spacer(),
      bslib::nav_item(
        shiny::tags$a(
          class = "nav-link cl-icon-link", href = "https://github.com/ebenogoe/clusnalyst",
          target = "_blank", rel = "noopener", `aria-label` = "Source code on GitHub",
          bsicons::bs_icon("github"), shiny::span(class = "d-lg-none ms-2", "Source code")
        )
      ),
      bslib::nav_item(bslib::input_dark_mode(id = "dark_mode", mode = "light")),
      footer = cl_footer()
    )
  )
}

analyse_ui <- function(lim) {
  bslib::layout_sidebar(
    class = "cl-analyse",
    fillable = FALSE,
    sidebar = bslib::sidebar(
      id = "steps_sidebar",
      width = 280,
      open = list(desktop = "open", mobile = "closed"),
      title = "Steps",
      shiny::uiOutput("stepper"),
      shiny::div(
        class = "cl-sidebar-foot",
        shiny::actionLink("start_tour", shiny::tagList(bsicons::bs_icon("signpost-split"), "Take the tour")),
        shiny::actionLink("reset_all", shiny::tagList(bsicons::bs_icon("arrow-counterclockwise"), "Start again")),
        if (lim$hosted) {
          shiny::p(class = "cl-hosted-note",
                   sprintf("This hosted copy takes files up to %d MB, aligns up to %d sequences and runs up to %s bootstrap replicates. Install the package to run bigger jobs on your own computer.",
                           lim$max_upload_mb, lim$max_sequences, fmt_n(lim$max_bootstrap)))
        }
      )
    ),
    shiny::div(
      class = "cl-mobile-steps d-lg-none",
      shiny::uiOutput("mobile_step")
    ),
    bslib::navset_hidden(
      id = "step",
      bslib::nav_panel_hidden("data", mod_data_ui("data")),
      bslib::nav_panel_hidden("qc", mod_qc_ui("qc")),
      bslib::nav_panel_hidden("distance", mod_distance_ui("distance")),
      bslib::nav_panel_hidden("tree", mod_tree_ui("tree")),
      bslib::nav_panel_hidden("validate", mod_validate_ui("validate")),
      bslib::nav_panel_hidden("groups", mod_groups_ui("groups")),
      bslib::nav_panel_hidden("report", mod_report_ui("report"))
    )
  )
}

cl_footer <- function() {
  shiny::tags$footer(
    class = "cl-footer",
    shiny::div(shiny::HTML(sprintf(
      'Clusnalyst %s. Created by Ebenezer Ogoe, Michael Obeng and Stephen Amoako (KNUST, 2022). Maintained by <a href="https://ebenezerogoe.com" target="_blank" rel="noopener">Ebenezer Ogoe</a>.',
      utils::packageVersion("clusnalyst")
    ))),
    shiny::div(
      shiny::tags$a(href = "https://github.com/ebenogoe/clusnalyst", target = "_blank", rel = "noopener", "Source"),
      shiny::tags$span(class = "cl-sep", "\u00b7"),
      shiny::tags$span("MIT licence")
    )
  )
}

#' Add external resources to the app
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path("www", app_sys("app/www"))
  shiny::tags$head(
    golem::favicon(ext = "svg"),
    shiny::tags$meta(name = "description",
                     content = "Cluster analysis of marker, trait and sequence data, with bootstrap support, cluster-number checks and a reproducible report."),
    golem::bundle_resources(path = app_sys("app/www"), app_title = "Clusnalyst"),
    cicerone::use_cicerone()
  )
}
