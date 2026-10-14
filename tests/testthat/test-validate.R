test_that("bootstrap support is a percentage per internal node and repeatable", {
  d <- clusnalyst_demo("binary")
  dm <- compute_distance(d)
  tr <- build_tree(dm, "upgma")
  b1 <- bootstrap_tree(d, tr, "jaccard", B = 20, seed = 7)
  b2 <- bootstrap_tree(d, tr, "jaccard", B = 20, seed = 7)
  expect_length(b1$support, tr$phylo$Nnode)
  expect_true(all(b1$support >= 0 & b1$support <= 100))
  expect_equal(b1$support, b2$support)
  expect_equal(b1$support[1], 100) # the root clade holds every sample
})

test_that("bootstrap leaves the caller's random numbers alone", {
  set.seed(99)
  before <- .Random.seed
  d <- clusnalyst_demo("binary")
  tr <- build_tree(compute_distance(d), "upgma")
  bootstrap_tree(d, tr, "jaccard", B = 5, seed = 1)
  expect_identical(.Random.seed, before)
})

test_that("bootstrap resamples markers, not the distance matrix (2022 bug)", {
  d <- clusnalyst_demo("binary")
  tr <- build_tree(compute_distance(d), "upgma")
  b <- bootstrap_tree(d, tr, "jaccard", B = 3, seed = 1)
  expect_setequal(b$trees[[1]]$tip.label, sample_names(d))
})

test_that("optimal_k finds the simulated groups and works for NJ trees", {
  d <- filter_data(clusnalyst_demo("snp"), min_maf = 0.05)
  dm <- compute_distance(d)
  k <- optimal_k(dm, build_tree(dm, "upgma"), k_max = 6)
  expect_equal(k$best_silhouette, 3)
  expect_equal(k$clustering, "tree cut")
  knj <- optimal_k(dm, build_tree(dm, "nj"), k_max = 6)
  expect_equal(knj$clustering, "PAM")
  expect_equal(nrow(knj$table), 5)
})

test_that("Mantel test of a matrix with itself gives r = 1", {
  dm <- compute_distance(clusnalyst_demo("binary"))
  m <- mantel_test(dm, dm, permutations = 99)
  expect_equal(m$r, 1)
  expect_lt(m$p, 0.05)
})

test_that("geography from metadata", {
  d <- clusnalyst_demo("snp")
  g <- metadata_distance(d$meta, c("latitude", "longitude"))
  expect_equal(attr(g, "Size"), 60)
})
