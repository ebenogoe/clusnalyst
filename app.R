# Entry point for shinyapps.io. Deploy with dev/03_deploy.R.
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
clusnalyst::run_app()
