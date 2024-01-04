`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


set_names <- function(x, nms) {
  if (length(nms) == 1 && length(nms) != length(x)) {
    nms <- rep(nms, length(x))
  }
  names(x) <- nms
  x
}


normalize_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = TRUE)
}


ensure_package <- function(name, call = NULL) {
  if (!requireNamespace(name, quietly = TRUE)) {
    instructions <- paste(
      "Please try installing '{name}' by running (in an empty session)",
      'install.packages("{name}", repos = c("https://mrc-ide.r-universe.dev",',
      '"https://cloud.r-project.org")')
    if (getOption("hipercow.auto_install_missing_packages", TRUE)) {
      cli::cli_alert_info("Trying to install '{name}'")
      cli::cli_alert_info(paste(
        "To prevent this, set",
        "options(hipercow.auto_install_missing_packages = FALSE)"))
      repos <- c("https://mrc-ide.r-universe.dev",
                 CRAN = "https://cloud.r-project.org")
      utils::install.packages(name, repos = repos)
      if (!requireNamespace(name, quietly = TRUE)) {
        cli::cli_abort(
          c("Installation of '{name}' failed!",
            i = instructions),
          call = call)
      }
      cli::cli_alert_success("Installation of '{name}' successful")
    } else {
      cli::cli_abort(
        c("Package '{name}' is not available",
          i = instructions,
          i = paste("To automatically install missing packages, set",
                    "options(hipercow.auto_install_missing_packages = TRUE)")),
        call = call)
    }
  }
  getNamespace(name)
}


squote <- function(x) {
  sprintf("'%s'", x)
}


vnapply <- function(...) {
  vapply(..., FUN.VALUE = 1)
}


vcapply <- function(...) {
  vapply(..., FUN.VALUE = "")
}


na_omit <- function(x) {
  x[!is.na(x)]
}


saverds_if_different <- function(object, path) {
  skip <- file.exists(path) && identical(readRDS(path), object)
  if (!skip) {
    saveRDS(object, path)
  }
  !skip
}


format_bytes <- function(x) {
  if (x >= 1e6) {
    sprintf("%s MB", round(x / 1e6, 3))
  } else if (x >= 1e3) {
    sprintf("%s kB", round(x / 1e3, 3))
  } else {
    sprintf("%s bytes", x)
  }
}


package_version_if_installed <- function(name) {
  tryCatch(utils::packageVersion(name),
           error = function(e) NULL)
}


eval_with_hr <- function(expr, title, verbose) {
  if (verbose) {
    cli::cli_rule(right = "{title} {cli::symbol$arrow_down}")
    ## Best to leave a blank line at the end, otherwise the final line
    ## might not be terminated, then the horizontal rule looks very
    ## silly.
    on.exit({
      cli::cli_text()
      cli::cli_rule(right = "{title} {cli::symbol$arrow_up}")
    }, add = TRUE, after = FALSE)
  }
  force(expr)
}


deparse_simple <- function(expr, width = getOption("width", 80) - 20) {
  ret <- rlang::expr_deparse(expr, width = width)
  if (length(ret) > 1) {
    ret <- paste0(ret[[1]], " [...]")
  }
  ret
}


collector <- function() {
  envir <- new.env(parent = emptyenv())
  envir$data <- list()
  list(
    add = function(x) {
      envir$data <- c(envir$data, list(x))
    },
    get = function() {
      envir$data
    }
  )
}


show_collected_warnings <- function(warnings) {
  if (length(warnings) == 0) {
    return()
  }
  cli::cli_alert_warning("{length(warnings)} warning{?s} found:")
  msg <- vcapply(warnings, conditionMessage)
  msg_grouped <- rle(msg)
  i <- msg_grouped$lengths > 1
  if (any(i)) {
    msg <- msg_grouped$values
    msg[i] <- sprintf("%s (%d times)",
                      msg_grouped$values[i], msg_grouped$lengths[i])
  }
  nwarnings <- getOption("nwarnings", 50)
  cli::cli_li(utils::tail(msg, nwarnings))
  if (length(msg) > nwarnings) {
    cli::cli_alert_info("Only last {nwarnings} distinct warnings shown")
  }
}


append_lines <- function(text, path) {
  con <- file(path, "a")
  on.exit(close(con))
  writeLines(text, con)
}
