test_that("binary distances match ade4 (squared) on complete data", {
  d <- clusnalyst_demo("binary")
  d$x[is.na(d$x)] <- 0
  df <- as.data.frame(d$x)
  for (m in list(c("jaccard", 1), c("simple_matching", 2), c("dice", 5))) {
    ours <- as.matrix(compute_distance(d, m[1]))
    theirs <- as.matrix(ade4::dist.binary(df, method = as.integer(m[2])))^2
    expect_equal(unname(ours), unname(theirs), tolerance = 1e-12)
  }
  expect_equal(as.vector(compute_distance(d, "jaccard", sqrt_binary = TRUE)),
               as.vector(ade4::dist.binary(df, method = 1)), tolerance = 1e-12)
})

test_that("Jaccard by hand", {
  dm <- as.matrix(compute_distance(small_binary(), "jaccard"))
  # a vs b: shared bands m1, m5; present in either: m1, m2, m4, m5
  expect_equal(dm["a", "b"], 1 - 2 / 4)
})

test_that("missing values are skipped pairwise", {
  d <- small_binary()
  d$x["a", "m2"] <- NA
  dm <- as.matrix(compute_distance(d, "jaccard"))
  # a vs b without m2: shared m1, m5; either m1, m4, m5
  expect_equal(dm["a", "b"], 1 - 2 / 3)
})

test_that("IBS and Rogers distances by hand", {
  x <- rbind(p = c(0, 0, 2, 1), q = c(0, 1, 0, 1), r = c(2, 2, 2, NA))
  d <- clusnalyst:::new_cl_data("snp", x)
  ibs <- as.matrix(compute_distance(d, "ibs"))
  expect_equal(ibs["p", "q"], (0 + 1 + 2 + 0) / (2 * 4))
  expect_equal(ibs["p", "r"], (2 + 2 + 0) / (2 * 3))
  rog <- as.matrix(compute_distance(d, "rogers"))
  expect_equal(rog["p", "q"], sqrt((0 + 0.25 + 1 + 0) / 4))
})

test_that("Gower matches cluster::daisy and Euclidean refuses categories", {
  d <- clusnalyst_demo("traits")
  g <- compute_distance(d, "gower")
  df <- d$x
  expect_equal(as.vector(g), as.vector(cluster::daisy(df, metric = "gower")))
  expect_error(compute_distance(d, "euclidean"), "Gower")
  expect_equal(default_distance(d), "gower")
})

test_that("sequence distances", {
  dna <- clusnalyst_demo("dna")
  p <- compute_distance(dna, "raw")
  expect_equal(as.vector(p), as.vector(ape::dist.dna(ape::as.DNAbin(tolower(dna$x)), "raw", pairwise.deletion = TRUE)))
  x <- rbind(a = strsplit("MKV-L", "")[[1]], b = strsplit("MKAAL", "")[[1]], c = strsplit("MRV-L", "")[[1]])
  prot <- clusnalyst:::new_cl_data("protein", x, seqs = c(a = "MKV-L", b = "MKAAL", c = "MRV-L"), aligned = TRUE)
  dm <- as.matrix(compute_distance(prot, "p"))
  expect_equal(dm["a", "b"], 1 / 4)
  expect_equal(dm["a", "c"], 1 / 4)
})

test_that("unknown methods are refused", {
  expect_error(compute_distance(small_binary(), "ibs"), "not a distance")
})
