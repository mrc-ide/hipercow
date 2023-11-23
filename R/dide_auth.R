##' Deal with DIDE credentials
##'
##' @title DIDE credentials
##' @export
dide_authenticate <- function() {
  if (keyring::keyring_is_locked()) {
    keyring::keyring_unlock()
  }

  cli::cli_h1("Please enter your DIDE credentials")
  cli::cli_text(paste(
    "We need to know your DIDE username and password in order to log you",
    "into the cluster. This will be shared across all projects on this",
    "machine, with the username and password stored securely in your system",
    "keychain. You will have to run this command again on other computers"))
  cli::cli_text()
  cli::cli_text(paste(
    "Your DIDE password may differ from your Imperial password, and in some",
    "cases your username may also differ. If in doubt, perhaps try logging in",
    "at https://mrcdata.dide.ic.ac.uk/hpc and use the combination that",
    "works for you there."))
  cli::cli_text()

  if ("hermod/dide/username" %in% keyring::key_list()$service) {
    username_guess <- keyring::key_get("hermod/dide/username")
  } else {
    username_guess <- get_system_username()
  }
  username <- check_username(
    readline_with_default("DIDE username", username_guess))
  keyring::key_set("hermod/dide/password", username = username)
  password <- keyring::key_get("hermod/dide")

  cli::cli_text()
  cli::cli_text(paste(
    "I am going to try and log in with your password now, if this fails we",
    "can always try again, as failure is just the first step towards great",
    "success."))

  result <- tryCatch(api_client_login(username, password), error = identity)

  if (inherits(result, "error")) {
    keyring::key_delete("hermod/dide/password")
    cli::cli_abort(c(
      "That username/password combination did not work, I'm afraid",
      x = result$message,
      i = "Please try again with dide_authenticate()"))
  }
  keyring::key_set_with_value("hermod/dide/username", password = username)
}


##' @rdname dide_authenticate
##' @export
dide_credentials <- function() {
  tryCatch({
    username <- keyring::key_get("hermod/dide/username")
    password <- keyring::key_get("hermod/dide/password", username = username)
    class(password) <- "password"
    list(username = username, password = password)
  }, error = function(e) {
    cli::cli_abort(
      "Did not find your DIDE credentials, please run dide_authenticate()")
  })
}


##' @export
as.character.password <- function(x, ...) {
  "*******************"
}


##' @export
print.password <- function(x, ...) {
  print("*******************")
  invisible(x)
}


check_username <- function(username) {
  assert_scalar_character(username)
  username <- sub("^DIDE\\\\", "", username)
  if (username == "") {
    stop("Invalid empty username")
  }
  username
}
