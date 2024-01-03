windows_authenticate <- function() {
  if (keyring::keyring_is_locked()) {
    cli::cli_text(paste(
      "I need to unlock the system keychain in order to load and save your",
      "credentials.  This might differ from your DIDE password, and will be",
      "the password you use to log in to this particular machine"))
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
    readline_with_default("DIDE username", windows_guess_username()))
  keyring::key_set("hipercow/dide/password", username = username)
  password <- keyring::key_get("hipercow/dide/password", username = username)

  cli::cli_text()
  cli::cli_text(paste(
    "I am going to try and log in with your password now, if this fails we",
    "can always try again, as failure is just the first step towards great",
    "success."))

  result <- tryCatch(api_client_login(username, password), error = identity)

  if (inherits(result, "error")) {
    keyring::key_delete("hipercow/dide/password", username = username)
    cli::cli_abort(c(
      "That username/password combination did not work, I'm afraid",
      x = result$message,
      i = "Please try again with 'windows_authenticate()'"))
  }
  keyring::key_set_with_value("hipercow/dide/username", password = username)

  cli::cli_text("Excellent news! Everything seems to work!")
  invisible(credentials(username, password))
}


windows_username <- function() {
  windows_credentials()$username
}


windows_credentials <- function() {
  tryCatch({
    username <- keyring::key_get("hipercow/dide/username")
    password <- keyring::key_get("hipercow/dide/password", username = username)
    credentials(username, password)
  }, error = function(e) {
    cli::cli_abort(
      "Did not find your DIDE credentials, please run 'windows_authenticate()'")
  })
}


windows_guess_username <- function() {
  if ("hipercow/dide/username" %in% keyring::key_list()$service) {
    keyring::key_get("hipercow/dide/username")
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
