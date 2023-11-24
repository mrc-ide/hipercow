example_credentials <- function() {
  list(username = "bob", password = "secret")
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
