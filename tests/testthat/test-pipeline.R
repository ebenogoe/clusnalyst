test_that("run_analysis runs every step", {
  d <- clusnalyst_demo("binary")
  s <- default_settings(d)
  s$bootstrap$B <- 10
  res <- run_analysis(d, s)
  expect_s3_class(res$tree, "cl_tree")
  expect_length(res$groups, 30)
  expect_false(is.null(res$kres))
})

test_that("sessions round-trip to the same results", {
  for (nm in c("traits", "dna")) {
    d <- clusnalyst_demo(nm)
    s <- default_settings(d)
    s$bootstrap$B <- 10
    f <- tempfile(fileext = ".json")
    save_session(d, s, f)
    back <- load_session(f)
    a <- run_analysis(d, s)
    b <- run_analysis(back$data, back$settings)
    expect_equal(as.vector(a$dist), as.vector(b$dist))
    expect_equal(unname(a$groups), unname(b$groups))
    expect_equal(a$boot$support, b$boot$support)
  }
})

test_that("load_session refuses other JSON", {
  f <- tempfile(fileext = ".json")
  writeLines('{"hello": 1}', f)
  expect_error(load_session(f), "not a Clusnalyst session")
})

test_that("the R script parses and the report is a full HTML page", {
  d <- clusnalyst_demo("binary")
  s <- default_settings(d)
  s$bootstrap$B <- 10
  code <- build_script(s, list(demo = "binary"))
  expect_silent(parse(text = code))
  res <- run_analysis(d, s)
  f <- tempfile(fileext = ".html")
  build_report(res, s, f)
  html <- paste(readLines(f), collapse = "\n")
  expect_match(html, "<head>", fixed = TRUE)
  expect_match(html, "<svg", fixed = TRUE)
  expect_match(html, "Bootstrap support", fixed = TRUE)
})
