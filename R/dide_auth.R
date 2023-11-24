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

  username <- check_username(
    readline_with_default("DIDE username", dide_guess_username()))
  keyring::key_set("hermod/dide/password", username = username)
  password <- keyring::key_get("hermod/dide/password", username = username)

  cli::cli_text()
  cli::cli_text(paste(
    "I am going to try and log in with your password now, if this fails we",
    "can always try again, as failure is just the first step towards great",
    "success."))

  result <- tryCatch(api_client_login(username, password), error = identity)

  if (inherits(result, "error")) {
    keyring::key_delete("hermod/dide/password", username = username)
    cli::cli_abort(c(
      "That username/password combination did not work, I'm afraid",
      x = result$message,
      i = "Please try again with 'dide_authenticate()'"))
  }
  keyring::key_set_with_value("hermod/dide/username", password = username)
  invisible(credentials(username, password))
}


##' @rdname dide_authenticate
##' @export
dide_credentials <- function() {
  tryCatch({
    username <- keyring::key_get("hermod/dide/username")
    password <- keyring::key_get("hermod/dide/password", username = username)
    credentials(username, password)
  }, error = function(e) {
    cli::cli_abort(
      "Did not find your DIDE credentials, please run 'dide_authenticate()'")
  })
}


dide_guess_username <- function() {
  if ("hermod/dide/username" %in% keyring::key_list()$service) {
    keyring::key_get("hermod/dide/username")
  } else {
    get_system_username()
  }
}


credentials <- function(username, password) {
  list(username = username,
       password = structure(password, class = "password"))
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
