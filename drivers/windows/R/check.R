windows_check <- function(path = getwd()) {
  ok <- windows_check_credentials()
  ok <- windows_check_connection() && ok
  ## ok <- windows_check_path() && ok
  invisible(ok)
}


windows_check_credentials <- function() {
  credentials <- windows_check_credentials_found()
  if (is.null(credentials)) {
    FALSE
  } else {
    windows_check_credentials_correct(credentials)
  }
}


windows_check_credentials_found <- function() {
  credentials <- tryCatch(windows_credentials(), error = identity)
  if (inherits(credentials, "error")) {
    cli::cli_alert_danger(credentials$message)
    NULL
  } else {
    cli::cli_alert_success(
      "Found DIDE credentials for '{credentials$username}'")
    credentials
  }
}


windows_check_credentials_correct <- function(credentials) {
  result <- tryCatch(
    api_client_login(credentials$username, credentials$password),
    error = identity)
  if (inherits(result, "error")) {
    cli::cli_alert_danger("Failed to log into the portal with your credentials")
    cli::cli_alert_info(
      paste("If your password has expired, you will need to reset it",
            "and then rerun 'hipercow::windows_authenticate()' to",
            "update the copy in your keyring"),
      wrap = TRUE)
    cli::cli_alert_info("Original error message: {result$message}")
    FALSE
  } else {
    cli::cli_alert_success("DIDE credentials are correct")
    TRUE
  }
}


windows_check_connection <- function(timeout = 1) {
  result <- tryCatch({
    httr::HEAD("https://vault.dide.ic.ac.uk:8200", httr::timeout(timeout))
  }, error = identity)
  if (inherits(result, "error")) {
    cli::cli_alert_danger("Failed to make connection with private network")
    cli::cli_alert_info("Please check that you have ZScaler enabled")
    FALSE
  } else {
    cli::cli_alert_success("Connection to private network working")
    TRUE
  }
}
