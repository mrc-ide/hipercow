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


vlapply <- function(...) {
  vapply(..., FUN.VALUE = TRUE)
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


saverds_if_not_exists <- function(object, path) {
  if (!file.exists(path)) {
    saveRDS(object, path)
  }
}


file_create_if_not_exists <- function(path) {
  file.create(path[!file.exists(path)])
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


relative_workdir <- function(root_path, call = NULL) {
  workdir <- normalize_path(getwd())
  if (!fs::path_has_parent(workdir, root_path)) {
    cli::cli_abort(
      c("Working directory is not a subdirectory of the hipercow root",
        i = "Working: {workdir}",
        i = "Hipercow: {root_path}"),
      call = call)
  }
  as.character(fs::path_rel(workdir, root_path))
}


show_progress <- function(progress, call = NULL) {
  if (is.null(progress)) {
    getOption("hipercow.progress", rlang::is_interactive())
  } else {
    assert_scalar_logical(progress, call = call)
    progress
  }
}


timeout_value <- function(timeout, call = NULL) {
  if (is.null(timeout)) {
    getOption("hipercow.timeout", Inf)
  } else {
    timeout
  }
}


last <- function(x) {
  x[[length(x)]]
}


ordinal <- function(n) {
  if (n == 1 || n > 20 && n %% 10 == 1) {
    suffix <- "st"
  } else if (n == 2 || n > 20 && n %% 10 == 2) {
    suffix <- "nd"
  } else if (n == 3 || n > 20 && n %% 10 == 3) {
    suffix <- "rd"
  } else {
    suffix <- "th"
  }
  paste0(n, suffix)
}


pretty_dt <- function(dt, missing = "???") {
  if (is.na(dt)) {
    missing
  } else if (requireNamespace("prettyunits", quietly = TRUE)) {
    prettyunits::pretty_dt(dt)
  } else {
    format(dt, digits = 2)
  }
}


time_ago <- function(time, missing = "unknown time ago") {
  if (is.na(time)) {
    missing
  } else if (requireNamespace("prettyunits", quietly = TRUE)) {
    prettyunits::time_ago(time)
  } else {
    paste(format(Sys.time() - time, digits = 2), "ago")
  }
}

duration_to_minutes <- function(period, name = "testing") {
  fail_msg <- function() {
    cli::cli_abort(c(
      "Could not convert {period} to minutes for {name}",
      i = "Use integer minutes, or d,h,m combinations such as 2h30m or 40d"))
  }

  # If it didn't end in 'm', 'd', or 'h' then add an 'm'.

  if (!substring(period, nchar(period)) %in% c("d", "h", "m")) {
    period <- paste0(period, "m")
  }

  # Fail if 'm', 'd' or 'h' are repeated, or any other non-digits turn up,
  # or if we start with 'm', 'd' or 'h'

  digits <- as.character(0:9)
  mdh <- strsplit(period, "")[[1]]
  mdh <- mdh[!mdh %in% digits]

  if ((max(table(mdh)) > 1) || (!all(mdh %in% c("d", "h", "m"))) ||
      (substring(period, 1, 1) %in% c("d", "h", "m"))) {
    fail_msg()
  }

  # There must be an easier way to do this...

  minutes <- 0
  index <- 1
  current_val <- 0
  while (index <= nchar(period)) {
    ch <- substring(period, index, index)
    if (ch %in% digits) {
      current_val <- (current_val * 10) + as.integer(ch)
    } else {
      current_val <- current_val *
        ((ch == "m") + 60 * (ch == "h") + 1440 * (ch == "d"))
      minutes <- minutes + current_val
      current_val <- 0
    }
    index <- index + 1
  }
  minutes
}


format_datetime <- function(year, month, day, hour, minute, second) {
  format(to_posix_ct(
    sprintf("%s-%s-%s %s:%s:%s", year, month, day, hour, minute, second)),
    "%Y-%m-%d %H:%M:%S")
}

to_posix_ct <- function(s) {
  as.POSIXct(s, format = "%Y-%m-%d %H:%M:%S")
}


special_time <- function(name, now = Sys.time()) {
  dt <- unclass(as.POSIXlt(now))

  if (name == "tonight") { # If between 7pm and 3am, run. Otherwise wait for 7pm
    if ((dt$hour < 19) && (dt$hour >= 3)) {
      dt$hour <- 19
      dt$min <- 0
      dt$sec <- 0
    }

  } else if (name == "midnight") { # Will allow up to 3am again/
    if (dt$hour >= 3) {
      date <- as.Date(now) + 1
      dt <- unclass(as.POSIXlt(date))
    }

  } else if (name == "weekend") {
    date <- as.Date(now)
    if ((dt$wday < 6) && (dt$wday > 0)) {  # We'll allow launching on Sat/Sun
      date <- date + (6 - dt$wday)
      dt <- unclass(as.POSIXlt(date))
    }
  } else {
    cli::cli_abort("Unrecognised special time {name}")
  }

  to_posix_ct(format_datetime((1900 + dt$year), (1 + dt$mon), dt$mday,
                              dt$hour, dt$min, dt$sec))
}


find_vars <- function(expr, exclude = character()) {
  if (rlang::is_call(expr, "{")) {
    ret <- character()
    for (e in as.list(expr[-1])) {
      if (rlang::is_call(e, c("<-", "<<-", "="))) {
        ret <- c(ret, find_vars(e[[3]], exclude))
        exclude <- c(exclude, as.character(e[[2]]))
      } else {
        ret <- c(ret, find_vars(e, exclude))
      }
    }
    ret
  } else {
    setdiff(all.vars(expr), exclude)
  }
}


in_pkgdown <- function() {
  identical(Sys.getenv("IN_PKGDOWN"), "true")
}
