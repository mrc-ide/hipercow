example_credentials <- function(online = FALSE) {
  if (online) {
    path <- "~/.smbcredentials"
    if (!file.exists(path)) {
      testthat::skip("credential file not found")
    }
    dide_credentials_old(path, TRUE)
  } else {
    dide_credentials_old(list(username = "bob", password = "secret"), TRUE)
  }
}


mock_response <- function(code, ..., url = NULL, content = NULL) {
  dat <- list(status_code = code,
              url = url %||% "http://example.com/",
              ...)
  if (is.character(content)) {
    dat$content <- charToRaw(paste(content, collapse = "\n"))
  } else {
    dat$content <- content
  }
  class(dat) <- "response"
  dat
}


r6_private <- function(x) {
  x[[".__enclos_env__"]]$private
}


password <- function(x) {
  structure(x, class = "password")
}
