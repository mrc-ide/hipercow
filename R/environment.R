##' Create, update, list, view and delete environments.
##'
##' @title Manage environments
##'
##' @param name Name of the environment. The name `default` is
##'   special; this is the environment that will be used by default
##'   (hence the name!).
##'
##' @param sources Files to source before starting a task. These will
##'   be sourced into the global (or execution) environment of the
##'   task. The paths must be relative to the hermod root, not the
##'   working directory.
##'
##' @param packages Packages to be *attached* before starting a
##'   task. These will be loaded with `library()` before the `sources`
##'   are sourced.  If you need to attach a package *after* a script
##'   for some reason, just call `library` yourself within one of your
##'   `source` files.
##'
##' @param overwrite On environment creation, replace an environment
##'   with the same name.
##'
##' @param root A hermod root, or path to it. If `NULL` we search up
##'   your directory tree.
##'
##' @return Nothing, all are called for their side effects.
##'
##' @rdname hermod_environment
##' @export
hermod_environment_create <- function(name = "default", sources = NULL,
                                      packages = NULL, overwrite = TRUE,
                                      root = NULL) {
  root <- hermod_root(root)

  ret <- new_environment(name, sources, packages, root, rlang::current_env())

  ## I did wonder about doing this by saving environment as:
  ##   hermod/environments/values/<hash>
  ## and a mapping file
  ##   hermod/environments/names/<name> -> hash
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
}


##' @export
##' @rdname hermod_environment
hermod_environment_list <- function(root = NULL) {
  root <- hermod_root(root)
  union("default", dir(root$path$environments))
}


##' @export
##' @rdname hermod_environment
hermod_environment_delete <- function(name = "default", root = NULL) {
  root <- hermod_root(root)
  assert_scalar_character(name)
  cli::cli_alert_warning("Deleting environment '{name}' (if it existed)")
  unlink(file.path(root$path$environments, name))
}


##' @export
##' @rdname hermod_environment
hermod_environment_show <- function(name = "default", root = NULL) {
  root <- hermod_root(root)
  env <- environment_load(name, root, rlang::current_env())
  print(env)
}

##' @export
##' @rdname hermod_environment
hermod_environment_exists <- function(name = "default", root = NULL) {
  root <- hermod_root(root)
  assert_scalar_character(name)
  name == "default" || file.exists(file.path(root$path$environments, name))
}


##' @export
print.hermod_environment <- function(x, ...) {
  cli::cli_h1("hermod environment '{x$name}'")
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
  invisible(x)
}


environment_load <- function(name, root, call = NULL) {
  path <- ensure_environment_exists(name, root, call)
  if (is.null(path)) {
   new_environment(name, NULL, NULL, root)
  } else {
    readRDS(path)
  }
}


ensure_environment_exists <- function(name, root, call) {
  assert_scalar_character(name)
  path <- file.path(root$path$environments, name)
  if (!file.exists(path)) {
    if (name != "default") {
      cli::cli_abort(
        c("Environment '{name}' does not exist",
          i = "Valid options are: {squote(hermod_environment_list(root))}"),
        call = call)
    }
    path <- NULL
  }
  invisible(path)
}


new_environment <- function(name, sources, packages, root, call = NULL) {
  assert_scalar_character(name)
  if (!is.null(sources)) {
    assert_character(sources)
    err <- !file.exists(file.path(root$path$root, sources))
    if (any(err)) {
      cli::cli_abort(
        c("File{?s} in 'sources' not found: {squote(sources[err])}",
          i = "Looking relative to '{root$path$root}'"),
        call = call)
    }
  }
  if (!is.null(packages)) {
    assert_character(packages)
  }
  ret <- list(name = name,
              sources = sources,
              packages = packages)
  class(ret) <- "hermod_environment"
  ret
}


environment_apply <- function(name, envir, root, call = NULL) {
  env <- environment_load(name, root, call)
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
