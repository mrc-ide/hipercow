hipercow <- function() {
  nrow <- logo_cols$dim[[1]]
  ncol <- logo_cols$dim[[2]]
  ch <- rep(c("H", "I", "P", "E", "R", "C", "O", "W"), each = 10 * nrow)
  s <- paste0(logo_cols$open, ch, logo_cols$close)
  str <- apply(matrix(s, nrow = nrow), 1, paste, collapse = "")
  cat(paste0(str[-1], "\n", collapse = ""))
}
