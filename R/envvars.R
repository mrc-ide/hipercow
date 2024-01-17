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


prepare_envvars <- function(envvars, root, call = NULL) {
  if (is.null(envvars)) {
    return(NULL)
  }
  if (!inherits(envvars, "hipercow_envvars")) {
    ## We might be able to do:
    ## > envvars <- hipercow_envvars(!!!envvars)
    ## here, which will generally be ok, but slightly complicates the
    ## errors that we throw.
    cli::cli_abort("Expected a 'hipercow_envvars' object for 'envvars'")
  }
  ## Early exit prevents having to load keypair; this is always
  ## the same regardless of the chosen driver.
  if (!any(envvars$secret)) {
    return(envvars)
  }
  ## TODO: much like the case with resources, there's an assumption
  ## here that only one driver is configured, and we need to be
  ## careful about this once we have two plausible drivers (also to
  ## error properly if the user has no drivers configured).  In both
  ## cases, we really require that the task is submitted too, I think.
  driver <- NULL
  dat <- hipercow_driver_prepare(driver, root, call)
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
