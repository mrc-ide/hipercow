##' Create, update, list, view and delete environments.
##'
##' @title Manage environments
##'
##' @param name Name of the environment. The name `default` is
##'   special; this is the environment that will be used by default
##'   (hence the name!).  Environment names can contain letters,
##'   numbers, hyphens and underscores.
##'
##' @param sources Files to source before starting a task. These will
##'   be sourced into the global (or execution) environment of the
##'   task. The paths must be relative to the hipercow root, not the
##'   working directory.  Files are sourced in order.
##'
##' @param packages Packages to be *attached* before starting a
##'   task. These will be loaded with `library()`, in the order
##'   provided, before the `sources` are sourced.  If you need to
##'   attach a package *after* a script for some reason, just call
##'   `library` yourself within one of your `source` files.
##'
##' @param globals Names of global objects that we can assume exist
##'   within this environment.  This might include function
##'   definitions or large data objects.  The special value `TRUE`
##'   triggers automatic detection of objects within your environment
##'   (this takes a few seconds and requires that the environment is
##'   constructable on your local machine too, so is not currently
##'   enabled by default).
##'
##' @param overwrite On environment creation, replace an environment
##'   with the same name.
##'
##' @param check Logical, indicating if we should check the source
##'   files for issues.  Pass `FALSE` here if you need to bypass these
##'   checks but beware the consequences that may await you.
##'
##' @param root A hipercow root, or path to it. If `NULL` we search up
##'   your directory tree.
##'
##' @return Nothing, all are called for their side effects.
##'
##' @rdname hipercow_environment
##' @seealso A longer introduction in `vignette("environments")`
##' @export
##' @examples
##' cleanup <- hipercow_example_helper()
##'
##' # Suppose you have a file with some functions you want to use in
##' # your task:
##' writeLines("simulation <- function(n) cumsum(rnorm(n))", "myfuns.R")
##'
##' # Update the default environment to include these functions (or in
##' # this example, just this one function)
##' hipercow_environment_create(sources = "myfuns.R")
##'
##' # You can now use this function in your tasks:
##' id <- task_create_expr(simulation(5))
##' task_wait(id)
##' task_result(id)
##'
##' cleanup()
hipercow_environment_create <- function(name = "default", packages = NULL,
                                        sources = NULL, globals = NULL,
                                        overwrite = TRUE, check = TRUE,
                                        root = NULL) {
  root <- hipercow_root(root)

  assert_scalar_character(name)
  if (name == "empty") {
    cli::cli_abort("Can't create environment with special name 'empty'",
                   name = "name")
  }
  check_safe_name_for_filename(name, "environment", rlang::current_env())

  ret <- new_environment(name, packages, sources, globals, check,
                         root, rlang::current_env())

  ## I did wonder about doing this by saving environment as:
  ##   hipercow/environments/values/<hash>
  ## and a mapping file
  ##   hipercow/environments/names/<name> -> hash
  ##
  ## then saving only the hash. But this leans back towards the sort
  ## of thing that *I* think is important and clearly not the users;
  ## in this case the ability for them to update their environment but
  ## have already-queued jobs run in the same one.  We could always
  ## look at this later, but literally noone wants it!
  path <- file.path(root$path$environments, name)
  exists <- file.exists(path)

  if (exists && identical(readRDS(path), ret)) {
    cli::cli_alert_info("Environment '{name}' is unchanged")
  } else if (exists && !overwrite) {
    cli::cli_abort(
      "Environment '{name}' already exists and 'overwrite' is FALSE")
  } else {
    fs::dir_create(dirname(path))
    saveRDS(ret, path)
    action <- if (exists) "Updated" else "Created"
    cli::cli_alert_success("{action} environment '{name}'")
  }

  if (name == "rrq" && is_rrq_enabled(root)) {
    cli::cli_alert_info("Refreshing existing rrq worker environments")
    controller <- hipercow_rrq_controller(root)
    rrq::rrq_worker_refresh(controller)
  }
}


##' @export
##' @rdname hipercow_environment
hipercow_environment_list <- function(root = NULL) {
  root <- hipercow_root(root)
  union(c("default", "empty"), dir(root$path$environments))
}


##' @export
##' @rdname hipercow_environment
hipercow_environment_delete <- function(name = "default", root = NULL) {
  root <- hipercow_root(root)
  assert_scalar_character(name)
  cli::cli_alert_warning("Deleting environment '{name}' (if it existed)")
  unlink(file.path(root$path$environments, name))
}


##' @export
##' @rdname hipercow_environment
hipercow_environment_show <- function(name = "default", root = NULL) {
  root <- hipercow_root(root)
  env <- environment_load(name, root, rlang::current_env())
  print(env)
}


##' @export
##' @rdname hipercow_environment
hipercow_environment_exists <- function(name = "default", root = NULL) {
  root <- hipercow_root(root)
  assert_scalar_character(name)
  name %in% c("default", "empty") ||
    file.exists(file.path(root$path$environments, name))
}


##' @export
print.hipercow_environment <- function(x, ..., header = TRUE) {
  if (header) {
    cli::cli_h1("hipercow environment '{x$name}'")
  }
  if (length(x$packages) == 0) {
    cli::cli_li("packages: {.emph (none)}")
  } else {
    pkgs <- cli::cli_vec(x$packages, list("vec-last" = ", "))
    cli::cli_li("packages: {.strong {pkgs}}")
  }
  if (length(x$sources) == 0) {
    cli::cli_li("sources: {.emph (none)}")
  } else {
    srcs <- cli::cli_vec(x$sources, list("vec-last" = ", "))
    cli::cli_li("sources: {.strong {srcs}}")
  }
  if (length(x$globals) == 0) {
    cli::cli_li("globals: {.emph (none)}")
  } else {
    srcs <- cli::cli_vec(x$globals, list("vec-last" = ", "))
    cli::cli_li("globals: {.strong {srcs}}")
  }
  invisible(x)
}


environment_load <- function(name, root = NULL, call = NULL) {
  root <- hipercow_root(root)
  path <- ensure_environment_exists(name, root, call)
  if (is.null(path)) {
    new_environment(name, NULL, NULL, NULL, FALSE, root)
  } else {
    readRDS(path)
  }
}


ensure_environment_exists <- function(name, root, call = NULL) {
  assert_scalar_character(name)
  path <- file.path(root$path$environments, name)
  if (!file.exists(path)) {
    if (!(name %in% c("default", "empty"))) {
      cli::cli_abort(
        c("Environment '{name}' does not exist",
          i = "Valid options are: {squote(hipercow_environment_list(root))}"),
        call = call)
    }
    path <- NULL
  }
  invisible(path)
}


new_environment <- function(name, packages, sources, globals, check, root,
                            call = NULL) {
  assert_scalar_character(name)
  if (!is.null(packages)) {
    assert_character(packages)
  }
  if (!is.null(sources)) {
    assert_character(sources)
    sources_full <- file.path(root$path$root, sources)
    err <- !file.exists(sources_full)
    if (any(err)) {
      cli::cli_abort(
        c("File{?s} in 'sources' not found: {squote(sources[err])}",
          i = "Looking relative to '{root$path$root}'"),
        call = call)
    }
    err <- fs::is_dir(sources_full)
    if (any(err)) {
      cli::cli_abort(
        c(paste("File{?s} in 'sources' is a directory, not a file:",
                "{squote(sources[err])}"),
          i = "Looking relative to '{root$path$root}'",
          i = paste("You cannot source a directory, only things that would be",
                    "valid inputs to R's function 'source()'")),
        call = call)
    }
    if (check) {
      environment_check_sources(sources_full, call)
    }
  }

  if (!is.null(globals)) {
    if (isTRUE(globals)) {
      globals <- discover_globals(name, packages, sources, root)
    } else {
      assert_character(globals)
    }
  }
  ret <- list(name = name,
              packages = packages,
              sources = sources,
              globals = globals)
  class(ret) <- "hipercow_environment"
  ret
}


environment_apply <- function(name, envir, root, call = NULL, verbose = FALSE) {
  if (is.list(name)) {
    env <- name
  } else {
    env <- environment_load(name, root, call)
  }
  if (verbose && (length(env$packages) > 0 || length(env$sources) > 0)) {
    cli::cli_alert_info("Loading environment '{env$name}'...")
    print(env, header = FALSE)
  }
  for (p in env$packages) {
    library(p, character.only = TRUE)
  }
  if (length(env$sources) > 0) {
    withr::local_dir(root$path$root)
    for (f in env$sources) {
      sys.source(f, envir = envir)
    }
  }
}


discover_globals <- function(name, packages, sources, root) {
  cli::cli_alert_info(
    "Creating '{name}' in a clean R session; this may take a moment")
  res <- callr::r(function(name, packages, sources, path_root) {
    envir <- new.env(parent = topenv())
    root <- hipercow_root(path_root)
    env <- new_environment(name, packages, sources, NULL, FALSE, root)
    environment_apply(env, envir, root)
    names(envir)
  }, list(name, packages, sources, root$path$root), package = TRUE)
  n <- length(res)
  cli::cli_alert_success("Found {n} {cli::qty(n)}symbol{?s}")
  res
}


check_globals <- function(globals, envir, call = call) {
  if (length(globals) == 0) {
    return()
  }
  values <- rlang::env_get_list(envir, names(globals), inherit = TRUE,
                                last = topenv())
  hashes <- vcapply(values, rlang::hash)
  err <- hashes != globals
  if (any(err)) {
    nms <- names(globals)[err]
    n <- length(err)
    hint <-
    cli::cli_abort(
      c("Unexpected value{?s} for global variable{?s}: {squote(nms)}",
        i = paste(
          "{cli::qty(n)}When we loaded your environment to run this task,",
          "the value of {?this variable/these variables} differed from the",
          "value we saw when saving the task originally.",
          "{?This variable/These variables} were likely created when",
          "sourcing your environment source scripts, so it's possible",
          "that you changed these scripts since creating the task?"),
        i = paste(
          "Disable this check at task creation by setting the option",
          "'hipercow.validate_globals' to FALSE")),
      call = call)
  }
}


environment_check_sources <- function(paths, call = NULL) {
  for (p in paths) {
    used <- all.names(parse(file = p))
    if ("install.packages" %in% used) {
      cli::cli_abort(
        c("Found call to 'install.packages()' in '{p}'",
          "!" = paste("Don't call install.packages() from any code that you",
                     "pass in as 'sources' when creating an environment.",
                     "This makes your tasks much slower and will corrupt your",
                     "library if you run more than one task at once"),
          i = paste("If this is a false positive, you can pass 'check = FALSE'",
                    "to 'hipercow_environment_create()' but it is probably",
                    "safer to move your package installation code into",
                    "another file")),
        call = call)
    }
  }
}
