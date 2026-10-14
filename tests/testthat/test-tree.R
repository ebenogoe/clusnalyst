test_that("each linkage label runs the matching hclust method (2022 bug)", {
  dm <- compute_distance(clusnalyst_demo("binary"))
  expect_equal(build_tree(dm, "single")$hclust$method, "single")
  expect_equal(build_tree(dm, "complete")$hclust$method, "complete")
  expect_equal(build_tree(dm, "upgma")$hclust$method, "average")
  expect_equal(build_tree(dm, "ward")$hclust$method, "ward.D2")
})

test_that("NJ trees are unrooted unless an outgroup is given", {
  dm <- compute_distance(clusnalyst_demo("dna"))
  tr <- build_tree(dm, "nj")
  expect_false(tr$rooted)
  expect_null(tr$hclust)
  og <- labels(dm)[1]
  rt <- build_tree(dm, "nj", outgroup = og)
  expect_true(rt$rooted)
  expect_true(ape::is.rooted(rt$phylo))
  expect_true(all(rt$phylo$edge.length >= 0))
})

test_that("cophenetic table is sorted and agrees with stats::cophenetic", {
  dm <- compute_distance(clusnalyst_demo("binary"))
  ct <- cophenetic_table(dm)
  expect_equal(nrow(ct), 7)
  expect_true(!is.unsorted(rev(stats::na.omit(ct$cophenetic_r))))
  hc <- stats::hclust(dm, "average")
  expect_equal(cophenetic_cor(dm, build_tree(dm, "upgma")), stats::cor(dm, stats::cophenetic(hc)))
})
