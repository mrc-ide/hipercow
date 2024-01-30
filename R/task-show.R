task_info_call_fn <- function(fn) {
  if (is.null(fn$name)) {
    "(anonymous)"
  } else if (is.null(fn$namespace)) {
    fn$name
  } else {
    sprintf("%s::%s", fn$namespace, fn$name)
  }
}


task_info_call_args <- function(args) {
  if (length(args) == 0) {
    "(none)"
  } else {
    ## Lots of ways to do this; this one at least shares some code
    ## with what we will eventually construct the call with, and
    ## should print reasonably nicely with truncation.
    call <- deparse_simple(rlang::call2("f", !!!args))
    gsub("(^f\\(|\\)$)", "", call)
  }
}
