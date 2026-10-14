tmp_file <- function(lines, ext) {
  f <- tempfile(fileext = ext)
  writeLines(lines, f)
  f
}

small_binary <- function() {
  m <- rbind(
    a = c(1, 1, 0, 0, 1),
    b = c(1, 0, 0, 1, 1),
    c = c(0, 0, 1, 1, 0),
    d = c(1, 1, 0, 0, 0)
  )
  colnames(m) <- paste0("m", 1:5)
  clusnalyst:::new_cl_data("binary", m)
}
