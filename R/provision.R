##' Provision a library.  This runs a small task on the cluster to set
##' up your packages.  If you have changed your R version you will
##' need to rerun this.  See `vignette("packages")` for much more on
##' this process.
##'
##' Our hope is that that most of the time you will not need to pass
##' any options through `...`, and that most of the time hipercow will
##' do the right thing. Please let us know if that is not the case and
##' you're having to routinely add arguments here.
##'
##' # Manually adding packages to an installation
##'
##' One case where we do expect that you will pass options through to
##' `hipercow_provision` is where you are manually adding packages to
##' an existing library.  The usage here will typically look like:
##'
##' ```
##' hipercow_provision("pkgdepends", refs = c("pkg1", "pkg2"))
##' ```
##'
##' where `pkg1` and `pkg2` are names of packages or pkgdepends
##' references (e.g., `username/repo` for a GitHub package; see
##' `vignette("packages")` for details).
##'
##' # Supported methods and options
##'
##' There are four possible methods: `pkgdepends`, `auto`, `script` and `renv`.
##'
##' The canonical source of documentation for all of these approaches
##' is `conan2::conan_configure`.
##'
##' ## `pkgdepends`
##'
##' The simplest method to understand, and probably most similar to
##' the approach in `didehpc`.  This method installs packages from a
##' list in `pkgdepends.txt` in your hipercow root, or via a vector of
##' provided package references. Uses
##' [pkgdepends](https://pkgdepends.r-lib.org) for the actual
##' dependency resolution and installation.
##'
##' Supported options (passed via `...`)
##'
##' * `refs`: A character vector of package references to override
##'   `pkgdepends.txt`
##' * `policy`: the policy argument to
##'   `pkgdepends::new_pkg_installation_proposal` (accepts `lazy` and
##'   `upgrade`)
##'
##' ## `auto`
##'
##' Uses `pkgdepends` internally but tries to do everything
##' automatically based on your declared environments (see
##' `hipercow_environment_create` and `vignette("hipercow")`) and the
##' installation information recorded in the locally installed
##' versions of the required packages.
##'
##' This is experimental and we'd love to know how it works for you.
##'
##' No options are supported, the idea is it's automatic :)
##'
##' ## `script`
##'
##' Runs a script (by default `provision.R`) on the cluster
##' to install things however you want.  Very flexible but you're on
##' your own mostly.  The intended use case of this option is where
##' `pkgdepends` fails to resolve your dependencies properly and you
##' need to install things manually.  The `remotes` package will be
##' pre-installed for you to use within your script.
##'
##' Your script will run on a special build queue, which will run even
##' when the cluster is very busy.  However, this is restricted in
##' other ways, allowing a maximum of 30 minutes and disallowing
##' parallel running.
##'
##' Supports one option:
##'
##' * `script`: The path for the script to run, defaulting to `provision.R`
##'
##' ## `renv`
##'
##' Uses [`renv`](https://rstudio.github.io/renv) to recreate your
##' renv environment.  You must be using `renv` locally for this to
##' work, and at present your renv project root must be the same as
##' your hipercow root.
##'
##' No options are currently supported, but we may pass some renv options
##' in the future; if you need more flexibility here please let us
##' know.
##'
##' @title Provision cluster library
##'
##' @param method The provisioning method to use, defaulting to
##'   `NULL`, which indicates we should try and detect the best
##'   provisioning mechanism for you; this should typically work well
##'   unless you are manually adding packages into your library (see
##'   Details). If given, must be one of `auto`, `pkgdepends`,
##'   `script` or `renv`; each of these are described in the Details
##'   and in `vignette("packages")`.
##'
##' @param ... Arguments passed through to conan. See Details.
##'
##' @param environment The name of the environment to provision (see
##'   [hipercow_environment_create] for details).
##'
##' @inheritParams task_submit
##'
##' @return Nothing
##'
##' @export
hipercow_provision <- function(method = NULL, ..., driver = NULL,
                               environment = "default", root = NULL) {
  ## TODO: here, if *no* driver is found that could be that we are
  ## running on the headnode, either by job submission or directly,
  ## and we'll need to handle that too.
  root <- hipercow_root(root)
  ensure_package("conan2", rlang::current_env())
  env <- environment_load(environment, root, rlang::current_env())
  args <- list(method = method, environment = env, ...)

  dat <- hipercow_driver_prepare(driver, root, rlang::current_env())
  dat$driver$provision_run(args, dat$config, root$path$root)
  invisible()
}


##' List previous successful installations of this hipercow root.
##'
##' @title List installations
##'
##' @inheritParams hipercow_provision
##'
##' @return A [data.frame] with columns:
##'
##' * `name`: the name of the installation. This might be useful with
##'   `conan_compare`
##' * `time`: the time the installation was started
##' * `hash`: the installation hash
##' * `method`: the method used for the installation
##' * `args`: the arguments to the installation (as a list column)
##' * `current`: if using `hipercow_provision_check`, does this
##'   installation match the arguments provided?
##'
##' This object also has class `conan_list` so that it prints nicely,
##'   but you can drop this with `as.data.frame`.
##'
##' @export
hipercow_provision_list <- function(driver = NULL, root = NULL) {
  root <- hipercow_root(root)
  ensure_package("conan2", rlang::current_env())
  dat <- hipercow_driver_prepare(driver, root, rlang::current_env())
  dat$driver$provision_list(NULL, dat$config, root$path$root)
}


##' @rdname hipercow_provision_list
##' @export
hipercow_provision_check <- function(method = NULL, ..., driver = NULL,
                                     environment = "default",
                                     root = NULL) {
  ## I don't think this is great, because it will perform poorly with
  ## multiple environments and requires a lot of care to get right.
  ##
  ## We might be interested in "have we ever provisioned?"  So if we
  ## returned a comparison of the different provisionings and if they
  ## match the hash that might be more useful?
  ##
  ## So
  root <- hipercow_root(root)
  ensure_package("conan2", rlang::current_env())
  env <- environment_load(environment, root, rlang::current_env())
  args <- list(method = method, environment = env, ...)
  dat <- hipercow_driver_prepare(driver, root, rlang::current_env())
  dat$driver$provision_list(args, dat$config, root$path$root)
}


##' Compare installations performed into your libraries by conan.
##'
##' @title Compare installations
##'
##' @param curr The previous installation to compare against. Can be a
##'   name (see [hipercow_provision_list] to get names), a negative
##'   number where `-n` indicates "`n` installations ago" or a
##'   positive number where `n` indicates "the `n`th
##'   installation". The default value of 0 corresponds to the current
##'   installation.
##'
##' @param prev The previous installation to compare against. Can be a
##'   name (see [hipercow_provision_list] to get names), a negative
##'   number where `-n` indicates "`n` installations ago" or a
##'   positive number where `n` indicates "the `n`th installation".
##'   The default of -1 indicates the previous installation. Must
##'   refer to an installation before `curr`. Use `NULL` or `-Inf` if
##'   you want to compare against the empty installation.
##'
##' @inheritParams hipercow_provision
##'
##' @return An object of class `conan_compare`, which can be printed
##'   nicely.
##'
##' @export
hipercow_provision_compare <- function(curr = 0, prev = -1, driver = NULL,
                                       root = NULL) {
  root <- hipercow_root(root)
  ensure_package("conan2", rlang::current_env())
  dat <- hipercow_driver_prepare(driver, root, rlang::current_env())
  dat$driver$provision_compare(dat$config, root$path$root, curr, prev)
}
