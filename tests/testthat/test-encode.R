test_that("letter genotypes are coded against the commonest allele", {
  m <- matrix(c("AA", "AG", "GG", "AA",
                "C/C", "C/T", "T|T", "NN",
                "A", "R", "G", "A"), nrow = 4)
  out <- genotypes_to_dosage(m)
  expect_equal(out[, 1], c(0, 1, 2, 0))
  expect_equal(out[, 2], c(0, 1, 2, NA))
  expect_equal(out[, 3], c(0, 1, 2, 0))
})

test_that("unknown codes become missing", {
  out <- genotypes_to_dosage(matrix(c("AA", "ZZ", "--", "0"), ncol = 1))
  expect_equal(out[, 1], c(0, NA, NA, NA))
})
