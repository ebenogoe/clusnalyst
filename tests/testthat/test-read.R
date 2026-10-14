test_that("formats are detected from content and extension", {
  expect_equal(detect_format(tmp_file(c("id,m1", "a,1"), ".csv")), "csv")
  expect_equal(detect_format(tmp_file(c("id\tm1", "a\t1"), ".txt")), "tsv")
  expect_equal(detect_format(tmp_file(c(">s1", "ACGT"), ".txt")), "fasta")
  expect_equal(detect_format(tmp_file(c("##fileformat=VCFv4.2"), ".txt")), "vcf")
  hm <- "rs#\talleles\tchrom\tpos\tstrand\tassembly#\tcenter\tprotLSID\tassayLSID\tpanelLSID\tQCcode\tS1"
  expect_equal(detect_format(tmp_file(c(hm, "m1\tA/G\t1\t10\t+\tNA\tNA\tNA\tNA\tNA\tNA\tAA"), ".hmp.txt")), "hapmap")
})

test_that("binary demo is read with samples in rows", {
  d <- read_markers(system.file("extdata", "demo_binary.csv", package = "clusnalyst"))
  expect_equal(d$type, "binary")
  expect_equal(dim(d$x), c(30, 60))
  expect_false(d$source$transposed)
})

test_that("a table with markers in rows is turned round", {
  d <- read_markers(system.file("extdata", "demo_snp.csv", package = "clusnalyst"))
  expect_equal(d$type, "snp")
  expect_true(d$source$transposed)
  expect_equal(dim(d$x), c(60, 1000))
  expect_true(all(d$x %in% c(0, 1, 2, NA)))
})

test_that("mixed traits keep categorical columns as factors", {
  d <- read_markers(system.file("extdata", "demo_traits.csv", package = "clusnalyst"))
  expect_equal(d$type, "quant")
  expect_s3_class(d$x$seed_coat, "factor")
  expect_type(d$x$plant_height_cm, "double")
})

test_that("duplicated sample IDs give a clear error", {
  f <- tmp_file(c("id,m1,m2", "a,1,0", "a,0,1", "b,1,1"), ".csv")
  expect_error(read_markers(f), "more than once")
})

test_that("VCF genotypes become dosages", {
  vcf <- c(
    "##fileformat=VCFv4.2",
    "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT\ts1\ts2\ts3",
    "1\t100\tsnp1\tA\tG\t.\tPASS\t.\tGT\t0/0\t0/1\t1/1",
    "1\t200\t.\tC\tT\t.\tPASS\t.\tGT\t0|1\t./.\t1|1"
  )
  d <- read_markers(tmp_file(vcf, ".vcf"))
  expect_equal(d$type, "snp")
  expect_equal(rownames(d$x), c("s1", "s2", "s3"))
  expect_equal(unname(d$x[, 1]), c(0, 1, 2))
  expect_equal(unname(d$x[, 2]), c(1, NA, 2))
  expect_equal(colnames(d$x)[2], "1_200")
})

test_that("HapMap files are read", {
  hm <- c(
    "rs#\talleles\tchrom\tpos\tstrand\tassembly#\tcenter\tprotLSID\tassayLSID\tpanelLSID\tQCcode\tS1\tS2\tS3",
    "m1\tA/G\t1\t10\t+\tNA\tNA\tNA\tNA\tNA\tNA\tAA\tAG\tGG",
    "m2\tC/T\t1\t20\t+\tNA\tNA\tNA\tNA\tNA\tNA\tC\tY\tNN"
  )
  d <- read_markers(tmp_file(hm, ".hmp.txt"))
  expect_equal(dim(d$x), c(3, 2))
  expect_equal(unname(d$x[, "m2"]), c(0, 1, NA))
})

test_that("FASTA type and alignment are recognised", {
  dna <- read_fasta(tmp_file(c(">a", "ACGT-ACGT", ">b", "ACGTTACGT", ">c", "ACGAAACGT"), ".fa"))
  expect_equal(dna$type, "dna")
  expect_true(dna$aligned)
  expect_equal(dim(dna$x), c(3, 9))
  prot <- read_fasta(tmp_file(c(">a", "MKVLAAGIVG", ">b", "MKVLSAGIV", ">c", "MRVLAAGWVGL"), ".fa"))
  expect_equal(prot$type, "protein")
  expect_false(prot$aligned)
  expect_null(prot$x)
})

test_that("metadata attaches in sample order", {
  d <- clusnalyst_demo("binary")
  expect_equal(rownames(d$meta), sample_names(d))
  expect_error(attach_metadata(d, data.frame(g = 1, row.names = "nobody")), "match")
})
