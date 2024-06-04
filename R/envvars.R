##' Create environment variables for use with a hipercow task.
##'
##' @title Environment variables
##'
##' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named environment
##'   variable.  If unnamed, it is assumed to refer to an environment
##'   variable that exists.
##'
##' @param secret Are these environment variables secret?  If so we will save
##'
##' @return A list with class `hipercow_envvars` which should not be modified.
##'
##' @export
##' @examples
##' # Declare environment variables as key-value pairs:
##' hipercow_envvars("MY_ENVVAR1" = "value1", "MY_ENVVAR2" = "value2")
##'
##' # If an environment variable already exists in your environment
##' # and you want to duplicate this into a task, you can use this
##' # shorthand:
##' Sys.setenv(HIPERCOW_EXAMPLE_ENVVAR = "moo") # suppose this exists already
##' hipercow_envvars("HIPERCOW_EXAMPLE_ENVVAR")
##' hipercow_envvars("HIPERCOW_EXAMPLE_ENVVAR", ANOTHER_ENVVAR = "value")
##'
##' # Secret envvars are still printed (at the moment at least) but
##' # once passed into a task they will be encrypted at rest.
##' hipercow_envvars("MY_SECRET" = "password", secret = TRUE)
##'
##' # Secret and public environment variables should be created
##' # separately and concatenated together:
##' env_public <- hipercow_envvars("HIPERCOW_EXAMPLE_ENVVAR")
##' env_secret <- hipercow_envvars("MY_PASSWORD" = "secret", secret = TRUE)
##' c(env_public, env_secret)
##'
##' # Cleanup
##' Sys.unsetenv("HIPERCOW_EXAMPLE_ENVVAR")
hipercow_envvars <- function(..., secret = FALSE) {
  assert_scalar_logical(secret)
  args <- rlang::dots_list(..., .named = FALSE)
  if (length(args) == 0) {
    return(make_envvars(character(), character(), logical()))
  }
  ok <- vlapply(args, rlang::is_scalar_atomic)
  if (!all(ok)) {
    cli::cli_abort("All arguments to 'hipercow_envvars' must be scalars")
  }
  value <- vcapply(args, as.character)
  name <- names(value)
  i <- !nzchar(name)
  if (any(i)) {
    name[i] <- value[i]
    value[i] <- Sys.getenv(vcapply(name[i], identity), NA_character_)
    if (any(is.na(value[i]))) {
      cli::cli_abort(
        c(paste("Failed to look up environment variables for",
                "unnamed arguments to 'hipercow_envvars'"),
          set_names(name[i][is.na(value[i])], "x")))
    }
  }
  make_envvars(name, value, secret)
}


##' @export
c.hipercow_envvars <- function(...) {
  inputs <- list(...)
  is_envvar <- vlapply(inputs, inherits, "hipercow_envvars")
  if (any(!is_envvar)) {
    cli::cli_abort("Can't combine 'hipercow_envvars' objects and other objects")
  }
  ret <- rlang::inject(rbind(!!!inputs))
  ret <- ret[!duplicated(ret$name, fromLast = TRUE), ]
  rownames(ret) <- NULL

  class(ret) <- c("hipercow_envvars", "data.frame")
  ret
}


make_envvars <- function(name, value, secret) {
  ret <- data.frame(name = as.character(name),
                    value = as.character(value),
                    secret = secret,
                    stringsAsFactors = FALSE)
  class(ret) <- c("hipercow_envvars", class(ret))
  ret
}


encrypt <- function(envvars, keypair) {
  f <- function(s) {
    openssl::base64_encode(openssl::rsa_encrypt(charToRaw(s), keypair$pub))
  }
  envvars$value[envvars$secret] <- vcapply(envvars$value[envvars$secret], f)
  attr(envvars, "key") <- keypair$key
  envvars
}


decrypt <- function(envvars) {
  key <- attr(envvars, "key", exact = TRUE)
  f <- function(s) {
    rawToChar(openssl::rsa_decrypt(openssl::base64_decode(s), key))
  }
  envvars$value[envvars$secret] <- vcapply(envvars$value[envvars$secret], f)
  attr(envvars, "key") <- NULL
  envvars
}


prepare_envvars <- function(envvars, driver, root, call = NULL) {
  if (!is.null(envvars) && !inherits(envvars, "hipercow_envvars")) {
    ## We might be able to do:
    ## > envvars <- hipercow_envvars(!!!envvars)
    ## here, which will generally be ok, but slightly complicates the
    ## errors that we throw.
    cli::cli_abort("Expected a 'hipercow_envvars' object for 'envvars'")
  }

  ## If a driver is not explicitly given (as in there is one
  ## unambiguous choice) we don't look up the environment variables.
  ## The driver loading here is all getting a bit tangled and we might
  ## look more carefully at this once we get the linux version going.
  if (is.null(driver)) {
    dat <- NULL
  } else {
    dat <- hipercow_driver_prepare(driver, root, call)
  }

  envvars <- envvars_combine(dat$driver$default_envvars, envvars)

  ## Early exit prevents having to load keypair; this is always
  ## the same regardless of the chosen driver.
  if (!any(envvars$secret)) {
    return(envvars)
  }
  if (is.null(driver)) {
    problem <- if (length(root$config) == 0) "configured" else "selected"
    cli::cli_abort(
      "No driver {problem}, so cannot work with secret environment variables",
      arg = "envvars", call = call)
  }

  keypair <- dat$driver$keypair(dat$config, root$path$root)
  encrypt(envvars, keypair)
}


envvars_apply <- function(envvars, envir) {
  if (is.null(envvars) || nrow(envvars) == 0) {
    return()
  }
  if (any(envvars$secret)) {
    envvars <- decrypt(envvars)
  }
  env <- set_names(envvars$value, envvars$name)
  withr::local_envvar(env, .local_envir = envir)
}

envvars_export <- function(envvars, path) {
  if (!is.null(envvars)) {
    vars <- envvars[!envvars$secret, ]
    writeLines(sprintf("%s=%s", vars$name, vars$value), path)
  } else {
    file.create(path)
  }
}


envvars_combine <- function(driver_envvars, task_envvars) {
  c(getOption("hipercow.default_envvars", DEFAULT_ENVVARS),
    driver_envvars,
    task_envvars)
}
