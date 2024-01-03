windows_check <- function() {
  windows_check_credentials()
  windows_check_connection()
}


windows_check_credentials <- function() {
  credentials <- windows_check_credentials_found()
  if (!is.null(credentials)) {
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
  } else {
    cli::cli_alert_success("DIDE credentials are correct")
  }
}


windows_check_connection <- function(timeout = 1) {
  result <- tryCatch({
    httr::HEAD("https://vault.dide.ic.ac.uk:8200", httr::timeout(timeout))
  }, error = identity)
  if (inherits(result, "error")) {
    cli::cli_alert_danger("Failed to make connection with private network")
    cli::cli_alert_info("Please check that you have ZScaler enabled")
  } else {
    cli::cli_alert_success("Connection to private network working")
  }
}
