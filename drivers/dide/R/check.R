dide_check <- function(path = getwd(), call = NULL) {
  ok <- dide_check_credentials()
  ok <- dide_check_connection() && ok
  ok <- dide_check_path(path) && ok
  ok <- dide_check_project(path) && ok
  ok <- dide_check_versions() && ok
  invisible(ok)
}


dide_check_credentials <- function() {
  credentials <- dide_check_credentials_found()
  if (is.null(credentials)) {
    FALSE
  } else {
    dide_check_credentials_correct(credentials)
  }
}


dide_check_credentials_found <- function() {
  credentials <- tryCatch(dide_credentials(), error = identity)
  if (inherits(credentials, "error")) {
    cli::cli_alert_danger(credentials$message)
    NULL
  } else {
    cli::cli_alert_success(
      "Found DIDE credentials for '{credentials$username}'")
    credentials
  }
}


dide_check_credentials_correct <- function(credentials) {
  result <- tryCatch(
    api_client_login(credentials$username, credentials$password),
    error = identity)
  if (inherits(result, "error")) {
    cli::cli_alert_danger(
      "Failed to log into the portal with your credentials")
    cli::cli_alert_info(
      paste("If your password has expired, you will need to reset it",
            "and then rerun 'hipercow::dide_authenticate()' to",
            "update the copy in your keyring"),
      wrap = TRUE)
    cli::cli_alert_info("Original error message: {result$message}")
    FALSE
  } else {
    cli::cli_alert_success("DIDE credentials are correct")
    TRUE
  }
}


dide_check_connection <- function(timeout = 1) {
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


dide_check_path <- function(path, mounts = detect_mounts()) {
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


dide_check_project <- function(path) {
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
        "Your working directory is a subdirectory of the project")
      TRUE
    } else {
      cli::cli_alert_danger(
        "Your working directory is not within your project, somehow")
      FALSE
    }
  }
}


## If this gets annoying we should probably do some clever versioning
## where we ignore patch numbers for this comparison (quite easy to
## do).
dide_check_versions <- function(v_hipercow = hipercow_version(),
                                   v_dide = hipercow_dide_version(),
                                   report_failure_only = FALSE) {
  if (v_hipercow == v_dide) {
    if (!report_failure_only) {
      cli::cli_alert_success(
        "'hipercow' and 'hipercow.dide' versions agree ({v_hipercow})")
    }
    return(TRUE)
  }
  cli::cli_alert_danger(
    paste("Your 'hipercow' ({v_hipercow}) and",
          "'hipercow.dide' ({v_dide}) versions differ"))
  cli::cli_alert_info(
    "You should install both again, in a fresh R session by running:")
  cmd <- paste(
    "install.packages(",
    '    c("hipercow", "hipercow.dide"),',
    '    repos = c("https://mrc-ide.r-universe.dev",',
    '              "https://cloud.r-project.org"))',
    sep = "\n")
  cli::cli_alert(gsub(" ", "\u00a0", cmd))
  FALSE
}
