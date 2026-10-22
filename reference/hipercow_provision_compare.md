# Compare installations

Compare installations performed into your libraries by conan.

## Usage

``` r
hipercow_provision_compare(curr = 0, prev = -1, driver = NULL, root = NULL)
```

## Arguments

- curr:

  The previous installation to compare against. Can be a name (see
  [hipercow_provision_list](https://mrc-ide.github.io/hipercow/reference/hipercow_provision_list.md)
  to get names), a negative number where `-n` indicates "`n`
  installations ago" or a positive number where `n` indicates "the `n`th
  installation". The default value of 0 corresponds to the current
  installation.

- prev:

  The previous installation to compare against. Can be a name (see
  [hipercow_provision_list](https://mrc-ide.github.io/hipercow/reference/hipercow_provision_list.md)
  to get names), a negative number where `-n` indicates "`n`
  installations ago" or a positive number where `n` indicates "the `n`th
  installation". The default of -1 indicates the previous installation.
  Must refer to an installation before `curr`. Use `NULL` or `-Inf` if
  you want to compare against the empty installation.

- driver:

  The name of the driver to use, or you can leave blank if only one is
  configured (this will be typical).

- root:

  The hipercow root

## Value

An object of class `conan_compare`, which can be printed nicely.

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper
hipercow_provision("pkgdepends", refs = "knitr")
#> /`-'\  _______  ___  ___ ____
#> \,T./ / __/ _ \/ _ \/ _ `/ _ \
#>   |   \__/\___/_//_/\_,_/_//_/
#>   |   ---- THE  LIBRARIAN ----
#> 
#> Bootstrapping from: /home/runner/work/_temp/Library
#> Installing into library: hipercow/lib
#> Using method pkgdepends
#> Running in path: /home/runner/work/_temp/hv-20261022-1a6a83dadbb
#> Library paths:
#>   - /home/runner/work/_temp/hv-20261022-1a6a83dadbb/hipercow/lib
#>   - /opt/R/4.6.1/lib/R/site-library
#>   - /opt/R/4.6.1/lib/R/library
#> id: 20261022120512
#> Logs from pkgdepends follow:
#> 
#> -------------------------------------------------------------------------------
#> 
#> 
#> ── repos 
#> • https://cloud.r-project.org
#> 
#> ── refs 
#> • knitr
#> 
#> ✔ Updated metadata database: 5.79 MB in 9 files.
#> 
#> ℹ Updating metadata database
#> ✔ Updating metadata database ... done
#> 
#> + evaluate   1.0.5  [bld][dl]
#> + highr      0.12   [bld][dl]
#> + knitr      1.52   [bld][dl]
#> + xfun       0.62   [bld][cmp][dl]
#> + yaml       2.3.12 [bld][cmp][dl]
#> ℹ Getting 5 pkgs with unknown sizes
#> ✔ Got highr 0.12 (source) (13.62 kB)
#> ✔ Got yaml 2.3.12 (source) (108.97 kB)
#> ✔ Got evaluate 1.0.5 (source) (39.32 kB)
#> ✔ Got xfun 0.62 (source) (229.69 kB)
#> ✔ Got knitr 1.52 (source) (490.01 kB)
#> ℹ Building evaluate 1.0.5
#> Error: Could not find tools necessary to compile a package
#> Call `pkgbuild::check_build_tools(debug = TRUE)` to diagnose the problem.
#> Execution halted
#> Error in (function (command = NULL, args = character(), error_on_status = TRUE,     wd = NULL, echo_cmd = FALSE, echo = FALSE, spinner = FALSE,     timeout = Inf, stdout = "|", stderr = "|", stdout_line_callback = NULL,     stdout_callback = NULL, stderr_line_callback = NULL, stderr_callback = NULL,     stderr_to_stdout = FALSE, stdin = NULL, env = NULL, windows_verbatim_args = FALSE,     windows_hide_window = FALSE, encoding = "", cleanup_tree = FALSE,     pty = FALSE, pty_options = list(), ...) {    assert_that(is_flag(error_on_status))    assert_that(is_time_interval(timeout))    assert_that(is_flag(spinner))    assert_that(is_string_or_null(stdout))    assert_that(is_string_or_null(stderr))    assert_that(is.null(stdout_line_callback) || is.function(stdout_line_callback))    assert_that(is.null(stderr_line_callback) || is.function(stderr_line_callback))    assert_that(is.null(stdout_callback) || is.function(stdout_callback))    assert_that(is.null(stderr_callback) || is.function(stderr_callback))    assert_that(is_flag(cleanup_tree))    assert_that(is_flag(stderr_to_stdout))    if (encoding == "binary") {        if (!is.null(stdout_line_callback)) {            throw(new_error("`stdout_line_callback` cannot be used with `encoding = \"binary\"`"))        }        if (!is.null(stderr_line_callback)) {            throw(new_error("`stderr_line_callback` cannot be used with `encoding = \"binary\"`"))        }    }    if (pty) {        if (!identical(stdout, "|")) {            throw(new_error("`stdout` must be `\"|\"` (the default) if `pty = TRUE`"))        }        if (!identical(stderr, "|")) {            throw(new_error("`stderr` must be `\"|\"` (the default) if `pty = TRUE`"))        }        if (stderr_to_stdout) {            throw(new_error("`stderr_to_stdout` must be `FALSE` if `pty = TRUE`"))        }        if (!is.null(stderr_callback)) {            throw(new_error("`stderr_callback` cannot be used with `pty = TRUE`"))        }        if (!is.null(stderr_line_callback)) {            throw(new_error("`stderr_line_callback` cannot be used with `pty = TRUE`"))        }        if (!is.null(stdin) && (!is_string(stdin) || stdin %in%             c("|", ""))) {            throw(new_error("When `pty = TRUE`, `stdin` must be `NULL` or a file path"))        }    }    "!DEBUG run() Checked arguments"    if (!interactive()) {        spinner <- FALSE    }    if (stderr_to_stdout) {        stderr <- "2>&1"    }    if (pty && is.character(stdin)) {        stdin_bytes <- readBin(stdin, "raw", n = file.info(stdin)$size)    }    else {        stdin_bytes <- NULL    }    pr <- process$new(command, args, echo_cmd = echo_cmd, wd = wd,         windows_verbatim_args = windows_verbatim_args, windows_hide_window = windows_hide_window,         stdin = if (pty)             NULL        else stdin, stdout = if (pty)             NULL        else stdout, stderr = if (pty)             NULL        else stderr, env = env, encoding = encoding, cleanup_tree = cleanup_tree,         pty = pty, pty_options = pty_options, ...)    "#!DEBUG run() Started the process: `pr$get_pid()`"    if (cleanup_tree) {        on.exit(pr$kill_tree(), add = TRUE)    }    else {        on.exit(pr$kill(), add = TRUE)    }    if (echo) {        stdout_callback <- echo_callback(stdout_callback, "stdout")        if (!pty)             stderr_callback <- echo_callback(stderr_callback,                 "stderr")    }    runcall <- sys.call()    resenv <- new.env(parent = emptyenv())    has_stdout <- pty || (!is.null(stdout) && stdout == "|")    has_stderr <- !pty && (!is.null(stderr) && stderr == "|")    binary <- encoding == "binary"    if (has_stdout) {        resenv$outbuf <- if (binary)             make_raw_buffer()        else make_buffer()        on.exit(resenv$outbuf$done(), add = TRUE)    }    if (has_stderr) {        resenv$errbuf <- if (binary)             make_raw_buffer()        else make_buffer()        on.exit(resenv$errbuf$done(), add = TRUE)    }    res <- tryCatch(run_manage(pr, timeout, spinner, stdout,         stderr, stdout_line_callback, stdout_callback, stderr_line_callback,         stderr_callback, resenv, binary, pty, stdin_bytes), interrupt = function(e) {        "!DEBUG run() process `pr$get_pid()` killed on interrupt"        out <- if (has_stdout) {            if (binary) {                resenv$outbuf$push(pr$read_output_bytes())                resenv$outbuf$push(pr$read_output_bytes())            }            else {                resenv$outbuf$push(pr$read_output())                resenv$outbuf$push(pr$read_output())            }            resenv$outbuf$read()        }        err <- if (has_stderr) {            if (binary) {                resenv$errbuf$push(pr$read_error_bytes())                resenv$errbuf$push(pr$read_error_bytes())            }            else {                resenv$errbuf$push(pr$read_error())                resenv$errbuf$push(pr$read_error())            }            resenv$errbuf$read()        }        tryCatch(pr$kill(), error = function(e) NULL)        signalCondition(new_process_interrupt_cond(list(interrupt = TRUE,             stderr = err, stdout = out, command = command, args = args),             runcall, echo = echo, stderr_to_stdout = stderr_to_stdout))        cat("\n")        invokeRestart("abort")    })    if (error_on_status && (is.na(res$status) || res$status !=         0)) {        "!DEBUG run() error on status `res$status` for process `pr$get_pid()`"        throw(new_process_error(res, call = sys.call(), echo = echo,             stderr_to_stdout, res$status, command = command,             args = args))    }    res})(...): ! System command 'Rscript' failed
hipercow_provision("pkgdepends", refs = "data.table")
#> /`-'\  _______  ___  ___ ____
#> \,T./ / __/ _ \/ _ \/ _ `/ _ \
#>   |   \__/\___/_//_/\_,_/_//_/
#>   |   ---- THE  LIBRARIAN ----
#> 
#> Bootstrapping from: /home/runner/work/_temp/Library
#> Installing into library: hipercow/lib
#> Using method pkgdepends
#> Running in path: /home/runner/work/_temp/hv-20261022-1a6a83dadbb
#> Library paths:
#>   - /home/runner/work/_temp/hv-20261022-1a6a83dadbb/hipercow/lib
#>   - /opt/R/4.6.1/lib/R/site-library
#>   - /opt/R/4.6.1/lib/R/library
#> id: 20261022120522
#> Logs from pkgdepends follow:
#> 
#> -------------------------------------------------------------------------------
#> 
#> 
#> ── repos 
#> • https://cloud.r-project.org
#> 
#> ── refs 
#> • data.table
#> ℹ Loading metadata database
#> ✔ Loading metadata database ... done
#> 
#> + data.table   1.18.6.1 [bld][cmp][dl]
#> ℹ Getting 1 pkg with unknown size
#> ✔ Got data.table 1.18.6.1 (source) (6.03 MB)
#> ℹ Building data.table 1.18.6.1
#> Error: Could not find tools necessary to compile a package
#> Call `pkgbuild::check_build_tools(debug = TRUE)` to diagnose the problem.
#> Execution halted
#> Error in (function (command = NULL, args = character(), error_on_status = TRUE,     wd = NULL, echo_cmd = FALSE, echo = FALSE, spinner = FALSE,     timeout = Inf, stdout = "|", stderr = "|", stdout_line_callback = NULL,     stdout_callback = NULL, stderr_line_callback = NULL, stderr_callback = NULL,     stderr_to_stdout = FALSE, stdin = NULL, env = NULL, windows_verbatim_args = FALSE,     windows_hide_window = FALSE, encoding = "", cleanup_tree = FALSE,     pty = FALSE, pty_options = list(), ...) {    assert_that(is_flag(error_on_status))    assert_that(is_time_interval(timeout))    assert_that(is_flag(spinner))    assert_that(is_string_or_null(stdout))    assert_that(is_string_or_null(stderr))    assert_that(is.null(stdout_line_callback) || is.function(stdout_line_callback))    assert_that(is.null(stderr_line_callback) || is.function(stderr_line_callback))    assert_that(is.null(stdout_callback) || is.function(stdout_callback))    assert_that(is.null(stderr_callback) || is.function(stderr_callback))    assert_that(is_flag(cleanup_tree))    assert_that(is_flag(stderr_to_stdout))    if (encoding == "binary") {        if (!is.null(stdout_line_callback)) {            throw(new_error("`stdout_line_callback` cannot be used with `encoding = \"binary\"`"))        }        if (!is.null(stderr_line_callback)) {            throw(new_error("`stderr_line_callback` cannot be used with `encoding = \"binary\"`"))        }    }    if (pty) {        if (!identical(stdout, "|")) {            throw(new_error("`stdout` must be `\"|\"` (the default) if `pty = TRUE`"))        }        if (!identical(stderr, "|")) {            throw(new_error("`stderr` must be `\"|\"` (the default) if `pty = TRUE`"))        }        if (stderr_to_stdout) {            throw(new_error("`stderr_to_stdout` must be `FALSE` if `pty = TRUE`"))        }        if (!is.null(stderr_callback)) {            throw(new_error("`stderr_callback` cannot be used with `pty = TRUE`"))        }        if (!is.null(stderr_line_callback)) {            throw(new_error("`stderr_line_callback` cannot be used with `pty = TRUE`"))        }        if (!is.null(stdin) && (!is_string(stdin) || stdin %in%             c("|", ""))) {            throw(new_error("When `pty = TRUE`, `stdin` must be `NULL` or a file path"))        }    }    "!DEBUG run() Checked arguments"    if (!interactive()) {        spinner <- FALSE    }    if (stderr_to_stdout) {        stderr <- "2>&1"    }    if (pty && is.character(stdin)) {        stdin_bytes <- readBin(stdin, "raw", n = file.info(stdin)$size)    }    else {        stdin_bytes <- NULL    }    pr <- process$new(command, args, echo_cmd = echo_cmd, wd = wd,         windows_verbatim_args = windows_verbatim_args, windows_hide_window = windows_hide_window,         stdin = if (pty)             NULL        else stdin, stdout = if (pty)             NULL        else stdout, stderr = if (pty)             NULL        else stderr, env = env, encoding = encoding, cleanup_tree = cleanup_tree,         pty = pty, pty_options = pty_options, ...)    "#!DEBUG run() Started the process: `pr$get_pid()`"    if (cleanup_tree) {        on.exit(pr$kill_tree(), add = TRUE)    }    else {        on.exit(pr$kill(), add = TRUE)    }    if (echo) {        stdout_callback <- echo_callback(stdout_callback, "stdout")        if (!pty)             stderr_callback <- echo_callback(stderr_callback,                 "stderr")    }    runcall <- sys.call()    resenv <- new.env(parent = emptyenv())    has_stdout <- pty || (!is.null(stdout) && stdout == "|")    has_stderr <- !pty && (!is.null(stderr) && stderr == "|")    binary <- encoding == "binary"    if (has_stdout) {        resenv$outbuf <- if (binary)             make_raw_buffer()        else make_buffer()        on.exit(resenv$outbuf$done(), add = TRUE)    }    if (has_stderr) {        resenv$errbuf <- if (binary)             make_raw_buffer()        else make_buffer()        on.exit(resenv$errbuf$done(), add = TRUE)    }    res <- tryCatch(run_manage(pr, timeout, spinner, stdout,         stderr, stdout_line_callback, stdout_callback, stderr_line_callback,         stderr_callback, resenv, binary, pty, stdin_bytes), interrupt = function(e) {        "!DEBUG run() process `pr$get_pid()` killed on interrupt"        out <- if (has_stdout) {            if (binary) {                resenv$outbuf$push(pr$read_output_bytes())                resenv$outbuf$push(pr$read_output_bytes())            }            else {                resenv$outbuf$push(pr$read_output())                resenv$outbuf$push(pr$read_output())            }            resenv$outbuf$read()        }        err <- if (has_stderr) {            if (binary) {                resenv$errbuf$push(pr$read_error_bytes())                resenv$errbuf$push(pr$read_error_bytes())            }            else {                resenv$errbuf$push(pr$read_error())                resenv$errbuf$push(pr$read_error())            }            resenv$errbuf$read()        }        tryCatch(pr$kill(), error = function(e) NULL)        signalCondition(new_process_interrupt_cond(list(interrupt = TRUE,             stderr = err, stdout = out, command = command, args = args),             runcall, echo = echo, stderr_to_stdout = stderr_to_stdout))        cat("\n")        invokeRestart("abort")    })    if (error_on_status && (is.na(res$status) || res$status !=         0)) {        "!DEBUG run() error on status `res$status` for process `pr$get_pid()`"        throw(new_process_error(res, call = sys.call(), echo = echo,             stderr_to_stdout, res$status, command = command,             args = args))    }    res})(...): ! System command 'Rscript' failed
hipercow_provision_compare()
#> Error in compare_select(path_lib, contents, curr, prev, rlang::current_env()): No conan installations found at
#> '/home/runner/work/_temp/hv-20261022-1a6a83dadbb/hipercow/lib'

cleanup()
#> ℹ Cleaning up example
```
