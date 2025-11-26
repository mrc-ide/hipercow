# Create a driver

Create a new hipercow driver; this is intended to be used from other
packages, and rarely called directly. If you are trying to run tasks on
a cluster you do not need to call this!

## Usage

``` r
hipercow_driver(
  configure,
  submit,
  status,
  info,
  log,
  result,
  cancel,
  provision_run,
  provision_list,
  provision_compare,
  keypair,
  check_hello,
  cluster_info,
  default_envvars = NULL
)
```

## Arguments

- configure:

  Function used to set core configuration for the driver. This function
  will be called from the hipercow root directory (so
  [`getwd()`](https://rdrr.io/r/base/getwd.html) will report the correct
  path). It can take any arguments, do any calculation and then must
  return any R object that can be serialised. The resulting
  configuration will be passed in as `config` to other driver functions.

- submit:

  Submit a task to a cluster. This is run after the task is created
  (either automatically or manually) and takes as arguments the task id,
  the configuration, the path to the root.

- status:

  Fetch a task status. Takes a vector of ids and returns a vector of the
  same length of statuses.

- info:

  Fetch task info for a single task. May take longer than `status` and
  expected to retrieve the true status from the scheduler.

- log:

  Fetch the task log. Takes a single task id and an integer (the number
  of lines already known) and returns a character vector of new logs.
  Return `NULL` (and not a zero length character vector) if a log is not
  available.

- result:

  Fetch a task result. If needed, copies the result file into the
  current hipercow root. Assume that a result is available (i.e., we've
  already checked that the task status is terminal)

- cancel:

  Cancel one or more tasks. Takes a vector of task ids, and requests
  that these tasks are cancelled, returning a list with elements
  `cancelled`: a logical vector the same length indicating if
  cancellation was successful, and `time_started`: the time that the
  task was started, or NA if the task was not yet started.

- provision_run:

  Provision a library. Works with conan, and must accept `args`,
  `config`, and `path_root`. The `args` should be injected into
  [`conan2::conan_configure`](https://mrc-ide.github.io/conan2/reference/conan_configure.html).
  It is expected this function will trigger running conan to provision a
  library. The return value is ignored, an error is thrown if the
  installation fails.

- provision_list:

  List previous installations. Takes `args` and if non-`NULL` injects
  into
  [`conan2::conan_configure`](https://mrc-ide.github.io/conan2/reference/conan_configure.html)
  (as for `provision_run`) in order to build a hash. Runs
  [`conan2::conan_list`](https://mrc-ide.github.io/conan2/reference/conan_list.html)
  returning its value.

- provision_compare:

  Test if a library is current. It is expected that this will call
  [`conan2::conan_compare`](https://mrc-ide.github.io/conan2/reference/conan_compare.html)

- keypair:

  Return a keypair as a list with elements `pub` and `key`; the public
  key as a string and the private key as a path that will be accessible
  when the cluster runs, but with permissions that are open only to the
  user who submitted the task.

- check_hello:

  Run any preflight checks before launching a hello world task. Return a
  validated resources list.

- cluster_info:

  Return information about a particular cluster: its maximum core count,
  maximum memory, node list and queue names, used for validating
  [hipercow_resources](https://mrc-ide.github.io/hipercow/reference/hipercow_resources.md)
  against that cluster.

- default_envvars:

  Driver-specific default environment variables. Drivers can use this to
  add environment variables that have a higher precedence than the
  hipercow defaults, but lower precedence than the
  `hipercow.default_envvars` option or the `envvars` argument to a task.
