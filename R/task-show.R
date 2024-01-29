task_show_expr <- function(expr, verbose) {
  if (verbose) {
    cli::cli_alert_info("expression: {deparse_simple(expr)}")
  }
}


task_show_locals <- function(locals, verbose) {
  if (verbose) {
    if (length(locals) == 0) {
      cli::cli_alert_info("no local variables")
    } else {
      n <- length(locals)
      nms <- squote(names(locals))
      cli::cli_alert_info(paste("exporting {n} local variable{?s} into the",
                                "execution environment: {nms}"))
    }
  }
}


task_show_script <- function(data, verbose) {
  if (verbose) {
  }
}


task_show_call_fn <- function(fn, verbose) {
  if (verbose) {
  }
}


task_show_call_args <- function(args, verbose) {
  if (verbose) {
  }
}
