##' Collects configuration information.
##'
##' @title Configuration
##'
##' @param credentials Either a list with elements username, password,
##'   or a path to a file containing lines `username=<username>`
##'   and `password=<password>` or your username (in which case
##'   you will be prompted graphically for your password).
##'
##' @param home Path to network home directory, on local system
##'
##' @param temp Path to network temp directory, on local system
##'
##' @param cluster Name of the cluster to use; one of
##'   [didehpc::valid_clusters()] or one of the aliases
##'   (small/little/dide/ide; big/mrc).
##'
##' @param shares Optional additional share mappings.  Can either be a
##'   single path mapping (as returned by [didehpc::path_mapping()]
##'   or a list of such calls.
##'
##' @param template A job template.  On fi--dideclusthn this can be
##'   "GeneralNodes" or "8Core". On "fi--didemrchnb" this can be
##'   "GeneralNodes", "12Core", "16Core", "12and16Core", "20Core",
##'   "24Core", "32Core", or "MEM1024" (for nodes with 1Tb of RAM; we have
##'   three - two of which have 32 cores, and the other is the AMD epyc with
##'   64). On the new "wpia-hn" cluster, you should
##'   currently use "AllNodes". See the main cluster documentation if you
##'   tweak these parameters, as you may not have permission to use
##'   all templates (and if you use one that you don't have permission
##'   for the job will fail).  For training purposes there is also a
##'   "Training" template, but you will only need to use this when
##'   instructed to.
##'
##' @param cores The number of cores to request.  If specified, then
##'   we will request this many cores from the windows queuer.  If you
##'   request too many cores then your task will queue forever!  24 is
##'   the largest this can be on fi--dideclusthn. On fi--didemrchnb,
##'   the GeneralNodes template has mainly 20 cores or less, with a
##'   single 64 core node, and the 32Core template has 32 core
##'   nodes. On wpia-hn, all the nodes are 32 core. If `cores` is
##'   omitted then a single core is assumed, unless `wholenode` is
##'   TRUE.
##'
##' @param wholenode If TRUE, request exclusive access to whichever
##'   compute node is allocated to the job. Your code will have access
##'   to all the cores and memory on the node.
##'
##' @param r_version A string, or `numeric_version` object, describing
##'   the R version required.  Not all R versions are known to be
##'   supported, so this will check against a list of installed R
##'   versions for the cluster you are using.  If omitted then: if
##'   your R version matches a version on the cluster that will be
##'   used, or the oldest cluster version that is newer than yours, or
##'   the most recent cluster version.
##'
##' @param use_java Logical, indicating if the script is going to
##'   require Java, for example via the rJava package.
##'
##' @param java_home A string, optionally giving the path of a
##'   custom Java Runtime Environment, which will be used if
##'   the use_java logical is true. If left blank, then the
##'   default cluster Java Runtime Environment will be used.
##'
##' @param workdir Here be dragons.
##'
##' @export
didehpc_config <- function(credentials = NULL, home = NULL, temp = NULL,
                           cluster = NULL, shares = NULL, template = NULL,
                           cores = NULL, wholenode = NULL, r_version = NULL,
                           use_java = NULL, java_home = NULL, workdir = NULL) {
  defaults <- didehpc_config_defaults()
  given <- list(credentials = credentials,
                home = home,
                temp = temp,
                cluster = cluster,
                shares = shares,
                template = template,
                cores = cores,
                wholenode = wholenode,
                workdir = workdir,
                r_version = r_version,
                use_java = use_java,
                java_home = java_home)
  dat <- modify_list(defaults,
                     given[!vapply(given, is.null, logical(1))])

  credentials <- dide_credentials(dat$credentials, FALSE)

  if (!is.null(dat$workdir)) {
    assert_scalar_character(dat$workdir, "workdir")
    if (!is_directory(dat$workdir)) {
      stop("workdir must be an existing directory")
    }
    workdir <- normalizePath(dat$workdir)
  }

  cluster <- cluster_name(dat$cluster)
  if (is.null(dat$template)) {
    dat$template <- valid_templates(cluster)[[1L]]
  }
  mounts <- detect_mount()
  remap_nas <- cluster %in% c("fi--didemrchnb", "wpia-hn")
  shares <- dide_detect_mount(mounts, dat$shares, dat$home, dat$temp,
                              dat$workdir, credentials$username,
                              remap_nas, cluster)
  resource <- check_resources(cluster, dat$template, dat$cores, dat$wholenode)

  dat$r_version <- select_r_version(dat$r_version)

  if (isTRUE(dat$use_java) && is.null(dat$java_home)) {
    dat$java_home <- ""
  }

  ret <- list(cluster = cluster,
              credentials = credentials,
              username = credentials$username,
              wholenode = dat$wholenode,
              workdir = workdir,
              resource = resource,
              shares = shares,
              r_version = dat$r_version,
              use_java = dat$use_java,
              java_home = dat$java_home)

  class(ret) <- "didehpc_config"
  ret
}


as_didehpc_config <- function(config) {
  if (!inherits(config, "didehpc_config")) {
    if (is.list(config)) {
      config <- do.call("didehpc_config", config)
    } else {
      stop("Expected a didehpc_config for 'config'")
    }
  }
  config
}


## TODO: I think we'll move away from options here, it's not very obvious.
didehpc_config_defaults <- function() {
  defaults <- list(
    cluster         = getOption("didehpc.cluster",         cluster_name(NULL)),
    credentials     = getOption("didehpc.credentials",     NULL),
    home            = getOption("didehpc.home",            NULL),
    temp            = getOption("didehpc.temp",            NULL),
    shares          = getOption("didehpc.shares",          NULL),
    template        = getOption("didehpc.template",        NULL),
    cores           = getOption("didehpc.cores",           NULL),
    wholenode       = getOption("didehpc.wholenode",       NULL),
    r_version       = getOption("didehpc.r_version",       NULL),
    workdir         = getOption("didehpc.workdir",         NULL),
    use_java        = getOption("didehpc.use_java",        FALSE),
    java_home       = getOption("didehpc.java_home",       NULL))

  if (is.null(defaults$credentials)) {
    username <- getOption("didehpc.username", NULL)
    if (!is.null(username)) {
      defaults$credentials <- username
    }
  }

  ## Extra shot for the windows users because we get the username
  ## automatically if they are on a domain machine.
  if (is_windows() && is.null(defaults$credentials)) {
    defaults$credentials <- Sys.getenv("USERNAME")
  }

  defaults
}


##' @export
print.didehpc_config <- function(x, ...) {
  cat("<didehpc_config>\n")
  expand <- c("numeric_version", "path_mapping")
  for (i in seq_along(x)) {
    el <- x[[i]]
    if (is.atomic(el) || inherits(el, expand)) {
      cat(sprintf(" - %s: %s\n", names(x)[[i]], as.character(el)))
    } else if (is.list(el)) {
      cat(sprintf(" - %s:\n", names(x)[[i]]))
      cat(paste(sprintf("    - %s: %s\n", names(el),
                        vcapply(el, as.character)), collapse = ""))
    }
  }
  invisible(x)
}



check_resources <- function(cluster, template, cores, wholenode) {
  template <- match_value(template, valid_templates(cluster))

  if (!is.null(cores)) {
    if (isTRUE(wholenode)) {
      stop("Cannot specify both wholenode and cores")
    }
    assert_scalar_integer(cores)
    if (cores > valid_cores(cluster)) {
      stop(sprintf("Maximum number of cores for %s is %d",
                   cluster, valid_cores(cluster)))
    }
    ret <- list(template = template, count = cores, type = "Cores")
  } else if (isTRUE(wholenode)) {
    ret <- list(template = template, count = 1L, type = "Nodes")
  } else {
    ret <- list(template = template, count = 1L, type = "Cores")
  }
  ret
}


select_r_version <- function(r_version, ours = getRversion()) {
  if (is.null(r_version)) {
    valid <- r_versions()
    if (ours %in% valid) {
      r_version <- numeric_version(ours)
    } else {
      i <- valid > ours
      j <- if (any(i)) which(i)[[1L]] else length(valid)
      r_version <- valid[[j]]
    }
  } else {
    if (is.character(r_version)) {
      r_version <- numeric_version(r_version)
    }
    if (!(r_version %in% r_versions())) {
      stop("Unsupported R version: ", as.character(r_version))
    }
  }
  r_version
}
