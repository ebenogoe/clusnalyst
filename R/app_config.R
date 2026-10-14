#' Path to a file inside the installed package
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "clusnalyst")
}

#' Read a value from inst/golem-config.yml
#'
#' shinyapps.io sets R_CONFIG_ACTIVE to "shinyapps", which switches on the
#' hosted limits.
#' @noRd
get_golem_config <- function(value,
                             config = Sys.getenv("GOLEM_CONFIG_ACTIVE", Sys.getenv("R_CONFIG_ACTIVE", "default")),
                             use_parent = TRUE, file = app_sys("golem-config.yml")) {
  config::get(value = value, config = config, file = file, use_parent = use_parent)
}

app_limits <- function() {
  list(
    hosted = isTRUE(get_golem_config("hosted")),
    max_upload_mb = get_golem_config("max_upload_mb"),
    max_sequences = get_golem_config("max_sequences"),
    max_bootstrap = get_golem_config("max_bootstrap")
  )
}
