init_quietly <- function(...) {
  suppressMessages(hermod_init(...))
}


mock_pkg <- function() {
  list(
    make_configuration = function(...) {
      list(...)
    })
}
