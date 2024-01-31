windows_check <- function(path = getwd()) {
  ok <- windows_check_credentials()
  ok <- windows_check_connection() && ok
  ok <- windows_check_path(path) && ok
  ok <- windows_check_project(path) && ok
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
    cli::cli_alert_danger(
      "Failed to log into the portal with your credentials")
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


windows_check_path <- function(path, mounts = detect_mounts()) {
  path <- normalize_path(path)
  result <- tryCatch(dide_add_extra_root_share(NULL, path, mounts),
                     error = identity)
  if (inherits(result, "error")) {
    cli::cli_alert_danger("Failed to map path to a network share")
    cli::cli_bullets(result$body)
    FALSE
  } else {
    cli::cli_alert_success("Path looks like it is on a network share")
    cli::cli_alert_info("Using '{path}'")
    TRUE
  }
}


windows_check_project <- function(path) {
  if (!rstudioapi::isAvailable()) {
    return(TRUE)
  }
  path_project <- rstudioapi::getActiveProject()
  if (is.null(path_project)) {
    cli::cli_alert_danger(
      "You are not using an RStudio project (but are using RStudio)")
    cli::cli_alert_info(
      paste("Using a project greatly reduces the amount of time you will",
            "think about paths, removes the need for setwd() and makes it",
            "more likely your project will work if you move the directory",
            "elsewhere."))
    TRUE
  } else {
    path_project <- normalize_path(path_project)
    ## We might want to do this against a hipercow root, if one exists?
    path <- normalize_path(path)
    cli::cli_alert_success("You are using an RStudio project")
    if (path_project == path) {
      cli::cli_alert_success("Your working directory is at the project root")
      TRUE
    } else if (fs::path_has_parent(path, path_project)) {
      cli::cli_alert_info(
        "Your working directory is a subdirectory of the the project")
      TRUE
    } else {
      cli::cli_alert_danger(
        "Your working directory is not within your project, somehow")
      FALSE
    }
  }
}
