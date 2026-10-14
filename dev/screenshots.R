# Takes the README and website screenshots by driving the installed app.
# Run from the package root after devtools::install(): Rscript dev/screenshots.R
library(shinytest2)
out <- "man/figures"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

app <- AppDriver$new(clusnalyst::run_app(), width = 1440, height = 1000, load_timeout = 60000,
                     timeout = 60000, seed = 1)
on.exit(app$stop())
go <- function(step) {
  app$run_js(sprintf("Shiny.setInputValue('goto_step', '%s', {priority: 'event'})", step))
  app$wait_for_idle(duration = 800)
}
shot <- function(name, selector = "html") {
  app$run_js("window.scrollTo(0, 0)")
  Sys.sleep(1)
  f <- file.path(out, paste0(name, ".png"))
  unlink(f)
  app$get_screenshot(f, selector = selector)
}

app$wait_for_idle(duration = 1500)
shot("01-home", "html")

go("data")
app$click("data-load_demo")
app$wait_for_idle(duration = 1000)
shot("02-data")

go("qc")
app$click("qc-apply")
app$wait_for_idle(duration = 1000)
shot("03-quality")

go("distance")
app$click("distance-go")
app$wait_for_idle(duration = 1500)
go("tree")
app$click("tree-compare")
app$wait_for_idle(duration = 800)
app$click("tree-go")
app$wait_for_idle(duration = 2000)

go("validate")
app$click("validate-run_boot")
app$wait_for_idle(duration = 1000)
app$set_inputs(`validate-gap` = TRUE)
app$click("validate-run_k")
app$wait_for_idle(duration = 1000)
shot("05-bootstrap")

go("groups")
app$click("groups-go")
app$wait_for_idle(duration = 1500)
shot("06-groups")

go("tree")
app$wait_for_idle(duration = 1500)
shot("04-tree")

go("report")
shot("07-report")
