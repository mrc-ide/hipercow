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

is_linux <- function() {
  Sys.info()[["sysname"]] == "Linux"
}

hipercow_file <- function(file) {
  system.file(file, package = "hipercow", mustWork = TRUE)
}

read_lines <- function(...) {
  paste(readLines(...), collapse = "\n")
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

duration_to_minutes <- function(period, name = "testing", call = NULL) {
  assert_scalar(period, name = name, call = call)
  fail_msg <- function(reason) {
    cli::cli_abort(
      c("Invalid value for '{name}': {period}",
        x = reason,
        i = "Use integer minutes, or d,h,m combinations such as 2h30m or 40d"),
      call = call, arg = name)
  }

  if (is.numeric(period)) {
    if (!rlang::is_integerish(period)) {
      fail_msg("'{name}' is a non-integer number of minutes")
    }
    if (period < 0) {
      fail_msg("'{name}' is a negative number of minutes")
    }
    if (period == 0) {
      fail_msg("{name}' is zero minutes")
    }
    ret <- as.integer(period)
  } else if (is.character(period)) {
    ## Easy case; we were given a string integer
    if (grepl("^[0-9]+$", period)) {
      ret <- as.integer(period)
    } else {
      re <- "^(([0-9]+)d)?(([0-9]+)h)?(([0-9]+)m)?$"
      if (!grepl(re, period, ignore.case = TRUE)) {
        fail_msg("Failed to parse string into XhYdZm format")
      }

      d <- as.integer(sub(re, "\\2", period, ignore.case = TRUE)) * 1440
      h <- as.integer(sub(re, "\\4", period, ignore.case = TRUE)) * 60
      m <- as.integer(sub(re, "\\6", period, ignore.case = TRUE))

      ret <- (if (is.na(d)) 0 else d) +
        (if (is.na(h)) 0 else h) +
        (if (is.na(m)) 0 else m)
    }
  } else {
    fail_msg("'{name}' must be a number or a string representing a duration")
  }

  if (ret == 0) {
    fail_msg("'{name}' is zero minutes")
  }
  ret
}


special_time <- function(name, now = Sys.time()) {
  switch(name,
         tonight = special_time_tonight(now),
         midnight = special_time_midnight(now),
         weekend = special_time_weekend(now),
         cli::cli_abort("Unrecognised special time {name}"))
}


special_time_tonight <- function(now = Sys.time()) {
  dt <- as.POSIXlt(now)
  if (dt$hour > 19 || dt$hour < 3) {
    return(now)
  }
  dt$hour <- 19
  dt$min <- 0
  dt$sec <- 0
  as_time(dt, attr(now, "tzone") %||% "")
}

special_time_midnight <- function(now = Sys.time()) {
  dt <- as.POSIXlt(now)
  if (dt$hour < 3) {
    return(now) # or NULL?
  }
  as_time(as.Date(now) + 1, attr(now, "tzone") %||% "")
}


special_time_weekend <- function(now = Sys.time()) {
  dt <- as.POSIXlt(now)
  if (dt$wday %in% c(0, 6)) { # Americans, smh
    return(now)
  }
  as_time(as.Date(now) + (6 - dt$wday), attr(now, "tzone") %||% "")
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


readlines_if_exists <- function(path) {
  if (file.exists(path)) readLines(path) else NULL
}


find_directory_descend <- function(target, start = ".", limit = "/") {
  root <- normalize_path(limit)
  start <- normalize_path(start)

  f <- function(path) {
    if (dir.exists(file.path(path, target))) {
      return(path)
    }
    if (normalize_path(path) == root) {
      return(NULL)
    }
    parent <- normalize_path(file.path(path, ".."))
    if (parent == path) {
      return(NULL)
    }
    f(parent)
  }
  ret <- f(start)
  if (!(is.null(ret))) {
    ret <- normalize_path(ret)
  }
}


## Just makes the tests and dealing with dates and times marginally
## less terrible.  I am certain that the guiding principle of
## dates/times in R is "do the worst thing possible at every choice,
## then don't document it properly".
as_time <- function(...) {
  ret <- as.POSIXct(...)
  if (identical(attr(ret, "tzone", exact = TRUE), "")) {
    attr(ret, "tzone") <- NULL
  }
  ret
}


print_simple_s3 <- function(x, name = class(x)[[1]]) {
  cli::cli_h1(name)
  i <- vlapply(x, is.null)
  for (nm in names(x)[!i]) {
    cli::cli_li("{nm}: {x[[nm]] %||% ''}")
  }
  if (any(i)) {
    cli::cli_text("Unset: {squote(names(x)[i])}")
  }
  invisible(x)
}


check_safe_name_for_filename <- function(name, what, call = NULL) {
  assert_scalar_character(name, name = "name", call = call)
  if (!grepl("^[a-zA-Z0-9_-]+$", name)) {
    what_upper <- paste0(toupper(substr(what, 1, 1)),
                         substr(what, 2, nchar(what)))
    cli::cli_abort(
      c("Invalid {what} name '{name}'",
        i = paste("{what_upper} names can contain letters, numbers, hyphens",
                  "and underscores only")),
      arg = "name", call = call)
  }
}


df_rows <- function(d) {
  i <- vlapply(d, is.list)
  ret <- lapply(seq_len(nrow(d)), function(j) as.list(d[j, , drop = FALSE]))
  if (any(i)) {
    for (j in seq_along(ret)) {
      ret[[j]][i] <- lapply(ret[[j]][i], function(x) x[[1]])
    }
  }
  ret
}


unlist_character <- function(x) {
  c(character(), unlist(x))
}


unlist_times <- function(x) {
  if (length(x) == 0L) {
    empty_time()
  } else {
    i <- vapply(x, inherits, TRUE, "POSIXlt")
    x[i] <- lapply(x[i], as.POSIXct)
    ret <- vapply(x, as.numeric, numeric(1))
    attributes(ret) <- attributes(x[[1L]])
    ret
  }
}


empty_time <- function() {
  Sys.time()[-1]
}


maybe_unlink <- function(x, recursive = FALSE, dry_run = FALSE) {
  if (!dry_run) {
    unlink(x, recursive)
  } else {
    recurse <- if (recursive) " recursively."
    files <- paste0(x, recurse)
    names(files) <- rep("*", length(files))
    down <- cli::symbol$arrow_down
    up <- cli::symbol$arrow_up
    cli::cli_rule(right = "Dry run - files were not deleted {down}")
    cli::cli_bullets(files)
    cli::cli_rule(right = "Dry run - files were not deleted {up}")
  }
}
