#' Run the Clusnalyst app
#'
#' @param onStart,options,enableBookmarking,uiPattern Passed to
#'   [shiny::shinyApp()].
#' @param ... Options passed to the app as golem options.
#' @return A Shiny app object.
#' @export
#' @examples
#' if (interactive()) run_app()
run_app <- function(onStart = NULL, options = list(), enableBookmarking = NULL,
                    uiPattern = "/", ...) {
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui, server = app_server, onStart = onStart, options = options,
      enableBookmarking = enableBookmarking, uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
