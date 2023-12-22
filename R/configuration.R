##' Report on your hipercow configuration.  We will always want you to
##' post this along side any problems; it has lots of useful
##' information in it that will help us see how your set up is
##' configured.
##'
##' @title Report on hipercow configuration
##'
##' @inheritParams hipercow_configure
##'
##' @param show Display the configuration to the screen
##'
##' @return A list with a machine readable form of this information,
##'   invisibly.
##'
##' @export
hipercow_configuration <- function(show = TRUE, root = NULL) {
  root <- hipercow_root(root)
  data <- configuration_data(root)
  if (show) {
    configuration_render(data)
  }
  invisible(data)
}


configuration_platform <- function() {
  info <- Sys.info()
  list("R" = getRversion(),
       system = info[["sysname"]],
       host = info[["nodename"]],
       user = info[["user"]])
}


configuration_packages <- function() {
  hipercow <- package_version_if_installed("hipercow")

  nms <- c("hipercow.windows", "conan2", "logwatch")
  pkgs <- set_names(lapply(nms, package_version_if_installed), nms)

  notes <- c()
  for (pkg in c("hipercow.windows", "conan2")) {
    if (is.null(pkgs[[pkg]])) {
      notes <- c(notes, "x" = sprintf("%s is not installed", pkg))
    }
  }
  pkgs <- pkgs[!vapply(pkgs, is.null, TRUE)]
  warn_version <- !is.null(pkgs$hipercow.windows) &&
    pkgs$hipercow.windows != hipercow
  if (warn_version) {
    notes <- c(notes,
               "!" = "hipercow and hipercow.windows have different versions")
  }
  list(hipercow = hipercow, others = pkgs, notes = notes)
}


configuration_paths <- function(root) {
  list(root = root$path$root,
       working = getwd(),
       path = relative_workdir(root$path$root))
}


configuration_drivers <- function(root) {
  root$config
}


configuration_data <- function(root) {
  list(platform = configuration_platform(),
       packages = configuration_packages(),
       paths = configuration_paths(root),
       drivers = configuration_drivers(root))
}


configuration_render <- function(data) {
  cli::cli_h1("hipercow root at {data$paths$root}")
  configuration_render_paths(data$paths)
  configuration_render_platform(data$platform)
  configuration_render_packages(data$packages)
  configuration_render_drivers(data$drivers)
}


configuration_render_paths <- function(paths) {
  cli::cli_alert_success("Working directory '{paths$path}' within root")
}


configuration_render_platform <- function(platform) {
  cli::cli_alert_info(
    paste("R version {platform$R} on {platform$system}",
          "({platform$user}@{platform$host})"))
}


configuration_render_packages <- function(packages) {
  cli::cli_h2("Packages")
  cli::cli_alert_info("This is hipercow {packages$hipercow}")
  versions_str <- sprintf("%s (%s)",
                          names(packages$others),
                          vcapply(packages$others, format))
  cli::cli_alert_info("Installed: {paste(versions_str, collapse = ', ')}")
  cli::cli_bullets(packages$notes)
}


configuration_render_drivers <- function(drivers) {
  cli::cli_h2("Drivers")
  n <- length(drivers)
  if (n == 0) {
    cli::cli_alert_danger("No drivers configured")
  } else {
    cli::cli_alert_success(
      "{n} {cli::qty(n)}driver{?s} configured ({squote(names(drivers))})")
    for (nm in names(drivers)) {
      cli::cli_h3(nm)
      config <- drivers[[nm]]
      for (i in names(config)) {
        el <- format(config[[i]])
        cli::cli_li("{.strong {i}}: {el[[1]]}")
        cli::cli_bullets(el[-1])
      }
    }
  }
}
