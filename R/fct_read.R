#' Data types Clusnalyst works with
#'
#' @return A named character vector, labels as names.
#' @export
data_types <- function() {
  c(
    "Dominant or binary markers (0/1)" = "binary",
    "SNP genotypes" = "snp",
    "Quantitative or mixed traits" = "quant",
    "DNA sequences" = "dna",
    "Protein sequences" = "protein"
  )
}

type_label <- function(type) {
  names(data_types())[match(type, data_types())]
}

# Short word for the columns of each data type, used in summaries
unit_label <- function(type, n = 2) {
  one <- switch(type,
    binary = "marker", snp = "SNP", quant = "trait",
    dna = "site", protein = "site", "column"
  )
  if (n == 1) one else paste0(one, "s")
}

cl_abort <- function(...) {
  stop(paste0(...), call. = FALSE)
}

new_cl_data <- function(type, x = NULL, seqs = NULL, aligned = NA,
                        source = list(), meta = NULL) {
  structure(
    list(type = type, x = x, seqs = seqs, aligned = aligned,
         source = source, meta = meta),
    class = "cl_data"
  )
}

#' Sample names of a Clusnalyst dataset
#' @param d A `cl_data` object.
#' @return Character vector.
#' @export
sample_names <- function(d) {
  if (!is.null(d$x)) rownames(d$x) else names(d$seqs)
}

n_samples <- function(d) length(sample_names(d))

n_columns <- function(d) {
  if (!is.null(d$x)) ncol(d$x) else NA_integer_
}

#' @export
print.cl_data <- function(x, ...) {
  cat("<Clusnalyst data>\n")
  cat("  Type:    ", type_label(x$type), "\n")
  cat("  Samples: ", n_samples(x), "\n")
  if (!is.null(x$x)) {
    cat("  ", tools::toTitleCase(unit_label(x$type)), ": ", ncol(x$x), "\n", sep = "")
  }
  if (x$type %in% c("dna", "protein")) {
    cat("  Aligned: ", isTRUE(x$aligned), "\n")
  }
  if (!is.null(x$meta)) cat("  Metadata columns:", paste(names(x$meta), collapse = ", "), "\n")
  invisible(x)
}

#' Guess the format of a data file
#'
#' Looks at the file extension first and then at the first lines of the
#' file.
#'
#' @param path Path to the file.
#' @param name Original file name, if `path` is a temporary upload.
#' @return One of `"csv"`, `"tsv"`, `"excel"`, `"vcf"`, `"hapmap"` or
#'   `"fasta"`.
#' @export
detect_format <- function(path, name = basename(path)) {
  name <- tolower(name)
  ext <- tools::file_ext(sub("\\.gz$", "", name))
  if (ext %in% c("xlsx", "xls")) return("excel")
  if (ext == "vcf") return("vcf")
  con <- if (grepl("\\.gz$", name)) gzfile(path) else file(path)
  first <- tryCatch(readLines(con, n = 5, warn = FALSE), error = function(e) character())
  close(con)
  first <- sub("^\ufeff", "", first)
  if (!length(first)) cl_abort("The file is empty.")
  if (grepl("^##fileformat=VCF", first[1])) return("vcf")
  if (grepl("^>", first[1])) return("fasta")
  if (ext %in% c("fasta", "fa", "fas", "fna", "faa", "ffn", "aln")) return("fasta")
  header <- tolower(first[1])
  if (grepl("^rs#?\t", header) && grepl("\talleles\t", header)) return("hapmap")
  tabs <- lengths(regmatches(header, gregexpr("\t", header)))
  commas <- lengths(regmatches(header, gregexpr(",", header)))
  semis <- lengths(regmatches(header, gregexpr(";", header)))
  if (tabs > commas && tabs >= semis) return("tsv")
  if (semis > commas) return("csv2")
  "csv"
}

na_strings <- c("", "NA", "na", "N/A", "n/a", "?", "NaN", "missing")

read_table_file <- function(path, format) {
  tab <- switch(format,
    excel = as.data.frame(readxl::read_excel(path, col_types = "text"),
                          check.names = FALSE, stringsAsFactors = FALSE),
    tsv = utils::read.delim(path, colClasses = "character", check.names = FALSE,
                            na.strings = na_strings, fileEncoding = "UTF-8-BOM"),
    csv2 = utils::read.csv(path, sep = ";", colClasses = "character", check.names = FALSE,
                           na.strings = na_strings, fileEncoding = "UTF-8-BOM"),
    utils::read.csv(path, colClasses = "character", check.names = FALSE,
                    na.strings = na_strings, fileEncoding = "UTF-8-BOM")
  )
  tab[] <- lapply(tab, function(v) {
    v <- trimws(v)
    v[v %in% na_strings] <- NA
    v
  })
  # Drop columns and rows that are completely empty (common in spreadsheets)
  keep_col <- vapply(tab, function(v) !all(is.na(v)), logical(1))
  tab <- tab[, keep_col, drop = FALSE]
  tab <- tab[rowSums(!is.na(tab)) > 0, , drop = FALSE]
  if (ncol(tab) < 2 || nrow(tab) < 2) {
    cl_abort("The table needs an ID column plus at least one data column, and at least two rows.")
  }
  tab
}

is_numberish <- function(v) {
  v <- v[!is.na(v)]
  length(v) > 0 && !anyNA(suppressWarnings(as.numeric(v)))
}

genotype_pattern <- "^([ACGTRYSWKMN]|[ACGTN-]{2}|[ACGTN][/|][ACGTN])$"

#' Guess the data type of a table
#'
#' @param tab A data frame whose first column holds IDs and whose other
#'   columns are character.
#' @return A list with `type` and `encoding`.
#' @export
guess_type <- function(tab) {
  vals <- unlist(tab[-1], use.names = FALSE)
  vals <- toupper(vals[!is.na(vals)])
  if (!length(vals)) cl_abort("The table has no data values.")
  u <- unique(vals)
  if (all(u %in% c("0", "1"))) return(list(type = "binary", encoding = "binary"))
  if (all(u %in% c("0", "1", "2"))) return(list(type = "snp", encoding = "dosage"))
  if (mean(grepl(genotype_pattern, u)) > 0.95 && !all(u %in% c("A", "C", "G", "T"))) {
    return(list(type = "snp", encoding = "letters"))
  }
  num <- vapply(tab[-1], is_numberish, logical(1))
  if (all(num)) return(list(type = "quant", encoding = "numeric"))
  list(type = "quant", encoding = "mixed")
}

guess_transpose <- function(tab, type) {
  # Marker tables almost always have more markers than samples, so a table
  # that is taller than it is wide probably has markers in rows.
  type %in% c("binary", "snp") && nrow(tab) > (ncol(tab) - 1)
}

#' Read a data file into a Clusnalyst dataset
#'
#' Reads CSV, TSV or Excel tables (first column = sample IDs), VCF, HapMap
#' and FASTA files. The data type and the table orientation are guessed
#' when not given.
#'
#' @param path Path to the file.
#' @param name Original file name (used for format detection).
#' @param format File format; guessed with [detect_format()] if `NULL`.
#' @param type Data type, one of [data_types()]; guessed if `NULL`.
#' @param transpose `TRUE` if samples are in columns and markers in rows.
#'   Guessed if `NULL`.
#' @return A `cl_data` object.
#' @export
#' @examples
#' f <- system.file("extdata", "demo_binary.csv", package = "clusnalyst")
#' read_markers(f)
read_markers <- function(path, name = basename(path), format = NULL,
                         type = NULL, transpose = NULL) {
  if (!file.exists(path)) cl_abort("File not found: ", name)
  format <- format %||% detect_format(path, name)
  src <- list(file = name, format = format)
  d <- switch(format,
    vcf = read_vcf(path),
    hapmap = read_hapmap(path),
    fasta = read_fasta(path, type = type),
    {
      tab <- read_table_file(path, format)
      guess <- guess_type(tab)
      type <- type %||% guess$type
      encoding <- if (identical(type, guess$type)) guess$encoding else NULL
      transpose <- transpose %||% guess_transpose(tab, type)
      src$encoding <- encoding
      src$transposed <- transpose
      table_to_data(tab, type, transpose, encoding)
    }
  )
  d$source <- utils::modifyList(src, d$source)
  d
}

`%||%` <- function(a, b) if (is.null(a)) b else a

table_to_data <- function(tab, type, transpose = FALSE, encoding = NULL) {
  ids <- tab[[1]]
  if (anyNA(ids)) cl_abort("Some rows have no ID in the first column.")
  if (anyDuplicated(ids)) {
    dup <- unique(ids[duplicated(ids)])
    cl_abort("These IDs appear more than once in the first column: ",
             paste(utils::head(dup, 5), collapse = ", "), ".")
  }
  m <- as.matrix(tab[-1])
  rownames(m) <- ids
  if (isTRUE(transpose)) m <- t(m)
  if (anyDuplicated(colnames(m))) colnames(m) <- make.unique(colnames(m))
  switch(type,
    binary = {
      v <- suppressWarnings(matrix(as.numeric(m), nrow(m), dimnames = dimnames(m)))
      bad <- !is.na(v) & !(v %in% c(0, 1))
      if (any(bad) || sum(is.na(v)) > sum(is.na(m))) {
        cl_abort("Binary data must contain only 0, 1 or blank cells.")
      }
      new_cl_data("binary", v)
    },
    snp = {
      enc <- encoding %||% (if (all(m[!is.na(m)] %in% c("0", "1", "2"))) "dosage" else "letters")
      v <- if (enc == "dosage") {
        suppressWarnings(matrix(as.numeric(m), nrow(m), dimnames = dimnames(m)))
      } else {
        genotypes_to_dosage(m)
      }
      if (any(!is.na(v) & !(v %in% 0:2))) cl_abort("SNP dosages must be 0, 1 or 2.")
      new_cl_data("snp", v)
    },
    quant = {
      df <- as.data.frame(m, stringsAsFactors = FALSE, check.names = FALSE)
      df[] <- lapply(df, function(v) {
        if (is_numberish(v)) as.numeric(v) else factor(v)
      })
      new_cl_data("quant", df)
    },
    cl_abort("Sequence data must be a FASTA file.")
  )
}

#' Read a VCF file as SNP dosages
#' @param path Path to a `.vcf` or `.vcf.gz` file.
#' @return A `cl_data` object of type `"snp"`.
#' @export
read_vcf <- function(path) {
  v <- vcfR::read.vcfR(path, verbose = FALSE)
  gt <- vcfR::extract.gt(v, element = "GT", as.numeric = FALSE)
  if (is.null(gt) || !length(gt)) cl_abort("No GT field found in the VCF file.")
  fix <- vcfR::getFIX(v)
  ids <- fix[, "ID"]
  ids[is.na(ids) | ids == "."] <- paste(fix[, "CHROM"], fix[, "POS"], sep = "_")[is.na(ids) | ids == "."]
  rownames(gt) <- make.unique(ids)
  dos <- gt_to_dosage(gt)
  new_cl_data("snp", t(dos), source = list(n_variants_in_file = nrow(gt)))
}

gt_to_dosage <- function(gt) {
  parts <- strsplit(gt, "[/|]")
  vals <- vapply(parts, function(p) {
    if (length(p) == 0 || anyNA(p) || any(p == ".")) return(NA_real_)
    # Count non-reference alleles; haploid calls are doubled
    n <- sum(p != "0")
    if (length(p) == 1) 2 * n else n
  }, numeric(1))
  matrix(vals, nrow(gt), ncol(gt), dimnames = dimnames(gt))
}

#' Read a HapMap genotype file
#' @param path Path to a HapMap (`.hmp.txt`) file.
#' @return A `cl_data` object of type `"snp"`.
#' @export
read_hapmap <- function(path) {
  tab <- utils::read.delim(path, colClasses = "character", check.names = FALSE)
  if (ncol(tab) < 12) cl_abort("A HapMap file needs 11 information columns followed by sample columns.")
  g <- as.matrix(tab[, 12:ncol(tab), drop = FALSE])
  rownames(g) <- make.unique(tab[[1]])
  new_cl_data("snp", genotypes_to_dosage(t(g)))
}

#' Read a FASTA file
#'
#' @param path Path to the file.
#' @param type `"dna"` or `"protein"`; guessed from the letters if `NULL`.
#' @return A `cl_data` object of type `"dna"` or `"protein"`.
#' @export
read_fasta <- function(path, type = NULL) {
  lines <- readLines(path, warn = FALSE)
  lines <- sub("^\ufeff", "", trimws(lines))
  lines <- lines[nzchar(lines) & !startsWith(lines, ";")]
  head_at <- which(startsWith(lines, ">"))
  if (!length(head_at)) cl_abort("No FASTA headers (lines starting with >) were found.")
  ends <- c(head_at[-1] - 1, length(lines))
  nm <- sub("^>\\s*", "", lines[head_at])
  nm <- sub("\\s.*$", "", nm)
  nm[!nzchar(nm)] <- paste0("seq", which(!nzchar(nm)))
  seqs <- mapply(function(s, e) {
    if (e <= s) "" else paste(lines[(s + 1):e], collapse = "")
  }, head_at, ends)
  seqs <- toupper(gsub("[[:space:]*]", "", seqs))
  names(seqs) <- make.unique(nm)
  seqs <- seqs[nzchar(seqs)]
  if (length(seqs) < 3) cl_abort("At least three sequences are needed for clustering.")
  if (is.null(type) || !type %in% c("dna", "protein")) {
    ch <- unlist(strsplit(gsub("[-.?NX]", "", paste(seqs, collapse = "")), ""))
    type <- if (length(ch) && mean(ch %in% c("A", "C", "G", "T", "U")) > 0.9) "dna" else "protein"
  }
  if (type == "dna") seqs <- chartr("U", "T", seqs)
  seqs <- gsub(".", "-", seqs, fixed = TRUE)
  aligned <- length(unique(nchar(seqs))) == 1
  d <- new_cl_data(type, seqs = seqs, aligned = aligned)
  if (aligned) d$x <- seqs_to_matrix(seqs)
  d
}

seqs_to_matrix <- function(seqs) {
  m <- do.call(rbind, strsplit(seqs, ""))
  rownames(m) <- names(seqs)
  colnames(m) <- seq_len(ncol(m))
  m
}

#' Align sequences with the msa package
#'
#' @param d A `cl_data` object of type `"dna"` or `"protein"`.
#' @param method `"ClustalOmega"`, `"ClustalW"` or `"Muscle"`.
#' @return The same object with an aligned matrix in `x`.
#' @export
align_sequences <- function(d, method = c("ClustalOmega", "ClustalW", "Muscle")) {
  method <- match.arg(method)
  if (!d$type %in% c("dna", "protein")) cl_abort("Only sequence data can be aligned.")
  raw <- gsub("-", "", d$seqs, fixed = TRUE)
  ss <- if (d$type == "dna") Biostrings::DNAStringSet(raw) else Biostrings::AAStringSet(raw)
  names(ss) <- names(raw)
  utils::capture.output(
    aln <- suppressMessages(msa::msa(ss, method = method, order = "input"))
  )
  out <- as.character(methods::as(aln, if (d$type == "dna") "DNAStringSet" else "AAStringSet"))
  names(out) <- names(raw)
  d$x <- seqs_to_matrix(out)
  d$aligned <- TRUE
  d$source$alignment <- method
  d
}

#' Read a sample metadata table
#'
#' @param path Path to a CSV, TSV or Excel file whose first column holds
#'   sample IDs.
#' @param name Original file name.
#' @return A data frame with sample IDs as row names.
#' @export
read_metadata <- function(path, name = basename(path)) {
  fmt <- detect_format(path, name)
  if (!fmt %in% c("csv", "csv2", "tsv", "excel")) cl_abort("Metadata must be a CSV, TSV or Excel table.")
  tab <- read_table_file(path, fmt)
  ids <- tab[[1]]
  if (anyDuplicated(ids)) cl_abort("Sample IDs in the metadata must be unique.")
  df <- tab[-1]
  df[] <- lapply(df, function(v) if (is_numberish(v)) as.numeric(v) else v)
  rownames(df) <- ids
  df
}

#' Attach metadata to a dataset
#' @param d A `cl_data` object.
#' @param meta A data frame from [read_metadata()].
#' @return `d` with `meta` holding one row per sample (in sample order).
#' @export
attach_metadata <- function(d, meta) {
  ids <- sample_names(d)
  hit <- intersect(ids, rownames(meta))
  if (!length(hit)) cl_abort("None of the metadata IDs match the sample names in the data.")
  m <- meta[match(ids, rownames(meta)), , drop = FALSE]
  rownames(m) <- ids
  d$meta <- m
  attr(d$meta, "matched") <- length(hit)
  d
}
