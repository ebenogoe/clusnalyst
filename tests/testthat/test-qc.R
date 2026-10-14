test_that("filtering works when no marker is monomorphic (2022 bug)", {
  d <- small_binary()
  f <- filter_data(d, max_marker_missing = 1, drop_monomorphic = TRUE)
  expect_equal(ncol(f$x), 5)
})

test_that("monomorphic and gappy markers are dropped", {
  d <- small_binary()
  d$x <- cbind(d$x, mono = 1, gappy = c(1, NA, NA, NA))
  q <- qc_summary(d)
  expect_true(q$markers$monomorphic[q$markers$marker == "mono"])
  f <- filter_data(d, max_marker_missing = 0.5)
  expect_false(any(c("mono", "gappy") %in% colnames(f$x)))
  expect_equal(f$filter$before[["columns"]], 7)
  expect_equal(f$filter$after[["columns"]], 5)
})

test_that("filtering that leaves nothing explains itself", {
  d <- small_binary()
  expect_error(filter_data(d, min_maf = 0.6), "No markers")
})

test_that("SNP QC reports MAF and heterozygosity", {
  q <- qc_summary(clusnalyst_demo("snp"))
  expect_true(all(q$markers$maf <= 0.5, na.rm = TRUE))
  expect_true(all(q$markers$heterozygosity >= 0 & q$markers$heterozygosity <= 1, na.rm = TRUE))
})
