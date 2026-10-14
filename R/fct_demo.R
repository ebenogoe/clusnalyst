#' Demo datasets
#'
#' Five small datasets for trying the app. `binary`, `snp` and `traits`
#' are simulated (see `data-raw/make_demos.R` in the source). `dna` is the
#' woodmouse cytochrome b alignment from the ape package, and `protein` is
#' the hemoglobin alpha example from the msa package.
#'
#' @return `demo_info()` returns a data frame describing each dataset.
#' @export
demo_info <- function() {
  data.frame(
    name = c("snp", "binary", "traits", "dna", "protein"),
    title = c(
      "SNP genotypes",
      "Dominant markers",
      "Mixed traits",
      "DNA alignment",
      "Protein sequences"
    ),
    description = c(
      "60 simulated lines from three populations, 1,000 SNPs as letter genotypes with markers in rows. Comes with population and location metadata.",
      "30 simulated accessions from three groups scored for 60 ISSR bands, with about 2% missing.",
      "40 simulated cowpea-like accessions with five measured traits and two categorical ones. Uses Gower distance.",
      "15 woodmouse cytochrome b sequences, 965 bp, already aligned (from the ape package).",
      "17 hemoglobin alpha sequences from vertebrates, not aligned (from the msa package). This is the example the 2022 app used."
    ),
    stringsAsFactors = FALSE
  )
}

#' @rdname demo_info
#' @param name One of `demo_info()$name`.
#' @return `clusnalyst_demo()` returns a `cl_data` object, with metadata
#'   where the demo has it.
#' @export
#' @examples
#' clusnalyst_demo("snp")
clusnalyst_demo <- function(name = c("snp", "binary", "traits", "dna", "protein")) {
  name <- match.arg(name)
  ext <- function(f) system.file("extdata", f, package = "clusnalyst", mustWork = TRUE)
  d <- switch(name,
    snp = read_markers(ext("demo_snp.csv")),
    binary = read_markers(ext("demo_binary.csv")),
    traits = read_markers(ext("demo_traits.csv")),
    dna = {
      env <- new.env()
      # ape's as.character() method for DNAbin only works once its namespace is loaded
      loadNamespace("ape")
      utils::data("woodmouse", package = "ape", envir = env)
      m <- toupper(as.character(env$woodmouse))
      colnames(m) <- seq_len(ncol(m))
      seqs <- apply(m, 1, paste, collapse = "")
      new_cl_data("dna", x = m, seqs = seqs, aligned = TRUE,
                  source = list(file = "ape::woodmouse", format = "fasta"))
    },
    protein = {
      f <- system.file("examples", "HemoglobinAA.fasta", package = "msa", mustWork = TRUE)
      read_fasta(f, type = "protein")
    }
  )
  meta <- switch(name, snp = "demo_snp_meta.csv", binary = "demo_binary_meta.csv",
                 traits = "demo_traits_meta.csv", NULL)
  if (!is.null(meta)) d <- attach_metadata(d, read_metadata(ext(meta)))
  d$source$file <- paste("Demo:", demo_info()$title[demo_info()$name == name])
  d$source$demo <- name
  d
}
