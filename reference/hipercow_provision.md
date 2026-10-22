# Provision cluster library

Provision a library. This runs a small task on the cluster to set up
your packages. If you have changed your R version you will need to rerun
this. See
[`vignette("packages")`](https://mrc-ide.github.io/hipercow/articles/packages.md)
for much more on this process.

## Usage

``` r
hipercow_provision(
  method = NULL,
  ...,
  driver = NULL,
  environment = "default",
  check_running_tasks = TRUE,
  root = NULL
)
```

## Arguments

- method:

  The provisioning method to use, defaulting to `NULL`, which indicates
  we should try and detect the best provisioning mechanism for you; this
  should typically work well unless you are manually adding packages
  into your library (see Details). If given, must be one of `auto`,
  `pkgdepends`, `script` or `renv`; each of these are described in the
  Details and in
  [`vignette("packages")`](https://mrc-ide.github.io/hipercow/articles/packages.md).

- ...:

  Arguments passed through to conan. See Details.

- driver:

  The name of the driver to use, or you can leave blank if only one is
  configured (this will be typical).

- environment:

  The name of the environment to provision (see
  [hipercow_environment_create](https://mrc-ide.github.io/hipercow/reference/hipercow_environment.md)
  for details).

- check_running_tasks:

  Logical, indicating if we should check that no tasks are running
  before starting installation. Generally, installing packages while
  tasks are running is harmful as you may get unexpected results, a task
  may start while a package is in an inconsistent state, and on windows
  you may get a corrupted library if a package is upgraded while it is
  loaded. You can disable this check by passing `FALSE`. Not all drivers
  respond to this argument, but the windows driver does.

- root:

  The hipercow root

## Value

Nothing

## Details

Our hope is that that most of the time you will not need to pass any
options through `...`, and that most of the time hipercow will do the
right thing. Please let us know if that is not the case and you're
having to routinely add arguments here.

## Manually adding packages to an installation

One case where we do expect that you will pass options through to
`hipercow_provision` is where you are manually adding packages to an
existing library. The usage here will typically look like:

    hipercow_provision("pkgdepends", refs = c("pkg1", "pkg2"))

where `pkg1` and `pkg2` are names of packages or pkgdepends references
(e.g., `username/repo` for a GitHub package; see
[`vignette("packages")`](https://mrc-ide.github.io/hipercow/articles/packages.md)
for details).

## Supported methods and options

There are four possible methods: `pkgdepends`, `auto`, `script` and
`renv`.

The canonical source of documentation for all of these approaches is
[`conan2::conan_configure`](https://mrc-ide.github.io/conan2/reference/conan_configure.html).

### `pkgdepends`

The simplest method to understand, and probably most similar to the
approach in `didehpc`. This method installs packages from a list in
`pkgdepends.txt` in your hipercow root, or via a vector of provided
package references. Uses [pkgdepends](https://pkgdepends.r-lib.org) for
the actual dependency resolution and installation.

Supported options (passed via `...`)

- `refs`: A character vector of package references to override
  `pkgdepends.txt`

- `policy`: the policy argument to
  [`pkgdepends::new_pkg_installation_proposal`](https://r-lib.github.io/pkgdepends/reference/pkg_installation_proposal.html)
  (accepts `lazy` and `upgrade`)

### `auto`

Uses `pkgdepends` internally but tries to do everything automatically
based on your declared environments (see `hipercow_environment_create`
and
[`vignette("hipercow")`](https://mrc-ide.github.io/hipercow/articles/hipercow.md))
and the installation information recorded in the locally installed
versions of the required packages.

This is experimental and we'd love to know how it works for you.

No options are supported, the idea is it's automatic :)

### `script`

Runs a script (by default `provision.R`) on the cluster to install
things however you want. Very flexible but you're on your own mostly.
The intended use case of this option is where `pkgdepends` fails to
resolve your dependencies properly and you need to install things
manually. The `remotes` package will be pre-installed for you to use
within your script.

Your script will run on a special build queue, which will run even when
the cluster is very busy. However, this is restricted in other ways,
allowing a maximum of 30 minutes and disallowing parallel running.

Supports one option:

- `script`: The path for the script to run, defaulting to `provision.R`

### `renv`

Uses [`renv`](https://rstudio.github.io/renv) to recreate your renv
environment. You must be using `renv` locally for this to work, and at
present your renv project root must be the same as your hipercow root.

No options are currently supported, but we may pass some renv options in
the future; if you need more flexibility here please let us know.

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper
writeLines(c("knitr", "data.table"), "pkgdepends.txt")
hipercow_provision()
#> ✔ Selected provisioning method 'pkgdepends'
#> /`-'\  _______  ___  ___ ____
#> \,T./ / __/ _ \/ _ \/ _ `/ _ \
#>   |   \__/\___/_//_/\_,_/_//_/
#>   |   ---- THE  LIBRARIAN ----
#> 
#> Bootstrapping from: /home/runner/work/_temp/Library
#> Installing into library: hipercow/lib
#> Using method pkgdepends
#> Running in path: /home/runner/work/_temp/hv-20261022-1a6a6f57f6c9
#> Library paths:
#>   - /home/runner/work/_temp/hv-20261022-1a6a6f57f6c9/hipercow/lib
#>   - /opt/R/4.6.1/lib/R/site-library
#>   - /opt/R/4.6.1/lib/R/library
#> id: 20261022120500
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
#> • data.table
#> 
#> ✔ Updated metadata database: 5.79 MB in 9 files.
#> 
#> ℹ Updating metadata database
#> ✔ Updating metadata database ... done
#> 
#> + data.table   1.18.6.1 [bld][cmp][dl]
#> + evaluate     1.0.5    [bld][dl]
#> + highr        0.12     [bld][dl]
#> + knitr        1.52     [bld][dl]
#> + xfun         0.62     [bld][cmp][dl]
#> + yaml         2.3.12   [bld][cmp][dl]
#> ℹ Getting 6 pkgs with unknown sizes
#> ✔ Got evaluate 1.0.5 (source) (39.32 kB)
#> ✔ Got highr 0.12 (source) (13.62 kB)
#> ✔ Got xfun 0.62 (source) (229.69 kB)
#> ✔ Got yaml 2.3.12 (source) (108.97 kB)
#> ✔ Got knitr 1.52 (source) (490.01 kB)
#> ✔ Got data.table 1.18.6.1 (source) (6.03 MB)
#> ℹ Building evaluate 1.0.5
#> Error: Could not find tools necessary to compile a package
#> Call `pkgbuild::check_build_tools(debug = TRUE)` to diagnose the problem.
#> Execution halted
#> Error in (function (command = NULL, args = character(), error_on_status = TRUE,     wd = NULL, echo_cmd = FALSE, echo = FALSE, spinner = FALSE,     timeout = Inf, stdout = "|", stderr = "|", stdout_line_callback = NULL,     stdout_callback = NULL, stderr_line_callback = NULL, stderr_callback = NULL,     stderr_to_stdout = FALSE, stdin = NULL, env = NULL, windows_verbatim_args = FALSE,     windows_hide_window = FALSE, encoding = "", cleanup_tree = FALSE,     pty = FALSE, pty_options = list(), ...) {    assert_that(is_flag(error_on_status))    assert_that(is_time_interval(timeout))    assert_that(is_flag(spinner))    assert_that(is_string_or_null(stdout))    assert_that(is_string_or_null(stderr))    assert_that(is.null(stdout_line_callback) || is.function(stdout_line_callback))    assert_that(is.null(stderr_line_callback) || is.function(stderr_line_callback))    assert_that(is.null(stdout_callback) || is.function(stdout_callback))    assert_that(is.null(stderr_callback) || is.function(stderr_callback))    assert_that(is_flag(cleanup_tree))    assert_that(is_flag(stderr_to_stdout))    if (encoding == "binary") {        if (!is.null(stdout_line_callback)) {            throw(new_error("`stdout_line_callback` cannot be used with `encoding = \"binary\"`"))        }        if (!is.null(stderr_line_callback)) {            throw(new_error("`stderr_line_callback` cannot be used with `encoding = \"binary\"`"))        }    }    if (pty) {        if (!identical(stdout, "|")) {            throw(new_error("`stdout` must be `\"|\"` (the default) if `pty = TRUE`"))        }        if (!identical(stderr, "|")) {            throw(new_error("`stderr` must be `\"|\"` (the default) if `pty = TRUE`"))        }        if (stderr_to_stdout) {            throw(new_error("`stderr_to_stdout` must be `FALSE` if `pty = TRUE`"))        }        if (!is.null(stderr_callback)) {            throw(new_error("`stderr_callback` cannot be used with `pty = TRUE`"))        }        if (!is.null(stderr_line_callback)) {            throw(new_error("`stderr_line_callback` cannot be used with `pty = TRUE`"))        }        if (!is.null(stdin) && (!is_string(stdin) || stdin %in%             c("|", ""))) {            throw(new_error("When `pty = TRUE`, `stdin` must be `NULL` or a file path"))        }    }    "!DEBUG run() Checked arguments"    if (!interactive()) {        spinner <- FALSE    }    if (stderr_to_stdout) {        stderr <- "2>&1"    }    if (pty && is.character(stdin)) {        stdin_bytes <- readBin(stdin, "raw", n = file.info(stdin)$size)    }    else {        stdin_bytes <- NULL    }    pr <- process$new(command, args, echo_cmd = echo_cmd, wd = wd,         windows_verbatim_args = windows_verbatim_args, windows_hide_window = windows_hide_window,         stdin = if (pty)             NULL        else stdin, stdout = if (pty)             NULL        else stdout, stderr = if (pty)             NULL        else stderr, env = env, encoding = encoding, cleanup_tree = cleanup_tree,         pty = pty, pty_options = pty_options, ...)    "#!DEBUG run() Started the process: `pr$get_pid()`"    if (cleanup_tree) {        on.exit(pr$kill_tree(), add = TRUE)    }    else {        on.exit(pr$kill(), add = TRUE)    }    if (echo) {        stdout_callback <- echo_callback(stdout_callback, "stdout")        if (!pty)             stderr_callback <- echo_callback(stderr_callback,                 "stderr")    }    runcall <- sys.call()    resenv <- new.env(parent = emptyenv())    has_stdout <- pty || (!is.null(stdout) && stdout == "|")    has_stderr <- !pty && (!is.null(stderr) && stderr == "|")    binary <- encoding == "binary"    if (has_stdout) {        resenv$outbuf <- if (binary)             make_raw_buffer()        else make_buffer()        on.exit(resenv$outbuf$done(), add = TRUE)    }    if (has_stderr) {        resenv$errbuf <- if (binary)             make_raw_buffer()        else make_buffer()        on.exit(resenv$errbuf$done(), add = TRUE)    }    res <- tryCatch(run_manage(pr, timeout, spinner, stdout,         stderr, stdout_line_callback, stdout_callback, stderr_line_callback,         stderr_callback, resenv, binary, pty, stdin_bytes), interrupt = function(e) {        "!DEBUG run() process `pr$get_pid()` killed on interrupt"        out <- if (has_stdout) {            if (binary) {                resenv$outbuf$push(pr$read_output_bytes())                resenv$outbuf$push(pr$read_output_bytes())            }            else {                resenv$outbuf$push(pr$read_output())                resenv$outbuf$push(pr$read_output())            }            resenv$outbuf$read()        }        err <- if (has_stderr) {            if (binary) {                resenv$errbuf$push(pr$read_error_bytes())                resenv$errbuf$push(pr$read_error_bytes())            }            else {                resenv$errbuf$push(pr$read_error())                resenv$errbuf$push(pr$read_error())            }            resenv$errbuf$read()        }        tryCatch(pr$kill(), error = function(e) NULL)        signalCondition(new_process_interrupt_cond(list(interrupt = TRUE,             stderr = err, stdout = out, command = command, args = args),             runcall, echo = echo, stderr_to_stdout = stderr_to_stdout))        cat("\n")        invokeRestart("abort")    })    if (error_on_status && (is.na(res$status) || res$status !=         0)) {        "!DEBUG run() error on status `res$status` for process `pr$get_pid()`"        throw(new_process_error(res, call = sys.call(), echo = echo,             stderr_to_stdout, res$status, command = command,             args = args))    }    res})(...): ! System command 'Rscript' failed
hipercow_provision_list()
#> ! No conan installations recorded

cleanup()
#> ℹ Cleaning up example
```
