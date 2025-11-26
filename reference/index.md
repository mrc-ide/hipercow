# Package index

## Get started

Probably the order you’ll work through when starting a new hipercow
project; create a new root, configure it, test that it works, and
provision any packages you need.

- [`hipercow_init()`](https://mrc-ide.github.io/hipercow/reference/hipercow_init.md)
  : Create a hipercow root
- [`hipercow_configure()`](https://mrc-ide.github.io/hipercow/reference/hipercow_configure.md)
  : Configure your hipercow root
- [`hipercow_configuration()`](https://mrc-ide.github.io/hipercow/reference/hipercow_configuration.md)
  : Report on hipercow configuration
- [`hipercow_hello()`](https://mrc-ide.github.io/hipercow/reference/hipercow_hello.md)
  : Hello world
- [`hipercow_unconfigure()`](https://mrc-ide.github.io/hipercow/reference/hipercow_unconfigure.md)
  : Remove a driver from a hipercow configuration

## Install packages

Your cluster tasks will most likely need some packages, and you need to
install them into a library.

- [`hipercow_provision()`](https://mrc-ide.github.io/hipercow/reference/hipercow_provision.md)
  : Provision cluster library
- [`hipercow_provision_list()`](https://mrc-ide.github.io/hipercow/reference/hipercow_provision_list.md)
  [`hipercow_provision_check()`](https://mrc-ide.github.io/hipercow/reference/hipercow_provision_list.md)
  : List installations
- [`hipercow_provision_compare()`](https://mrc-ide.github.io/hipercow/reference/hipercow_provision_compare.md)
  : Compare installations

## Tasks

Create and interact with tasks.

### Creation

- [`task_create_explicit()`](https://mrc-ide.github.io/hipercow/reference/task_create_explicit.md)
  : Create explicit task
- [`task_create_expr()`](https://mrc-ide.github.io/hipercow/reference/task_create_expr.md)
  : Create a task based on an expression
- [`task_create_call()`](https://mrc-ide.github.io/hipercow/reference/task_create_call.md)
  : Create task from call
- [`task_create_script()`](https://mrc-ide.github.io/hipercow/reference/task_create_script.md)
  : Create script task
- [`task_create_bulk_expr()`](https://mrc-ide.github.io/hipercow/reference/task_create_bulk_expr.md)
  : Create bulk tasks from an expression
- [`task_create_bulk_call()`](https://mrc-ide.github.io/hipercow/reference/task_create_bulk_call.md)
  : Create bulk tasks from a call

### Cluster resources and parallelism

- [`hipercow_resources()`](https://mrc-ide.github.io/hipercow/reference/hipercow_resources.md)
  : Hipercow Resources

- [`hipercow_resources_validate()`](https://mrc-ide.github.io/hipercow/reference/hipercow_resources_validate.md)
  :

  Validate a `hipercow_resources` list for a driver.

- [`hipercow_parallel()`](https://mrc-ide.github.io/hipercow/reference/hipercow_parallel.md)
  : Specify parallel use of cores

- [`hipercow_parallel_get_cores()`](https://mrc-ide.github.io/hipercow/reference/hipercow_parallel_get_cores.md)
  : Get number of cores

- [`hipercow_parallel_set_cores()`](https://mrc-ide.github.io/hipercow/reference/hipercow_parallel_set_cores.md)
  : Set various environment variables that report the number of cores
  available for execution.

### Interaction

- [`task_status()`](https://mrc-ide.github.io/hipercow/reference/task_status.md)
  : Get task status
- [`task_result()`](https://mrc-ide.github.io/hipercow/reference/task_result.md)
  : Get task result
- [`task_wait()`](https://mrc-ide.github.io/hipercow/reference/task_wait.md)
  : Wait for a task to complete
- [`task_retry()`](https://mrc-ide.github.io/hipercow/reference/task_retry.md)
  : Retry a task
- [`task_info()`](https://mrc-ide.github.io/hipercow/reference/task_info.md)
  : Fetch task information
- [`task_cancel()`](https://mrc-ide.github.io/hipercow/reference/task_cancel.md)
  : Cancel tasks
- [`task_list()`](https://mrc-ide.github.io/hipercow/reference/task_list.md)
  : List tasks

### Logs

- [`task_log_show()`](https://mrc-ide.github.io/hipercow/reference/task_log.md)
  [`task_log_value()`](https://mrc-ide.github.io/hipercow/reference/task_log.md)
  [`task_log_watch()`](https://mrc-ide.github.io/hipercow/reference/task_log.md)
  : Get task log

## Bundles of tasks

Bundles allow you to group related tasks together

- [`hipercow_bundle_create()`](https://mrc-ide.github.io/hipercow/reference/hipercow_bundle_create.md)
  : Create task bundle
- [`hipercow_bundle_load()`](https://mrc-ide.github.io/hipercow/reference/hipercow_bundle_load.md)
  : Load existing bundle
- [`hipercow_bundle_list()`](https://mrc-ide.github.io/hipercow/reference/hipercow_bundle_list.md)
  : List existing bundles
- [`hipercow_bundle_delete()`](https://mrc-ide.github.io/hipercow/reference/hipercow_bundle_delete.md)
  : Delete task bundles

### Interact with task bundles

These functions all have `task_` analogues

- [`hipercow_bundle_status()`](https://mrc-ide.github.io/hipercow/reference/hipercow_bundle_status.md)
  : Bundle status
- [`hipercow_bundle_result()`](https://mrc-ide.github.io/hipercow/reference/hipercow_bundle_result.md)
  : Fetch bundle results
- [`hipercow_bundle_wait()`](https://mrc-ide.github.io/hipercow/reference/hipercow_bundle_wait.md)
  : Wait for a bundle to complete
- [`hipercow_bundle_retry()`](https://mrc-ide.github.io/hipercow/reference/hipercow_bundle_retry.md)
  : Retry task bundle
- [`hipercow_bundle_cancel()`](https://mrc-ide.github.io/hipercow/reference/hipercow_bundle_cancel.md)
  : Cancel bundle tasks
- [`hipercow_bundle_log_value()`](https://mrc-ide.github.io/hipercow/reference/hipercow_bundle_log_value.md)
  : Fetch bundle logs

## Environments

Environments are the collections of packages and functions that we
construct on a cluster machine and evaluate your tasks within. These
functions let you create, update and delete these environments.

- [`hipercow_environment_create()`](https://mrc-ide.github.io/hipercow/reference/hipercow_environment.md)
  [`hipercow_environment_list()`](https://mrc-ide.github.io/hipercow/reference/hipercow_environment.md)
  [`hipercow_environment_delete()`](https://mrc-ide.github.io/hipercow/reference/hipercow_environment.md)
  [`hipercow_environment_show()`](https://mrc-ide.github.io/hipercow/reference/hipercow_environment.md)
  [`hipercow_environment_exists()`](https://mrc-ide.github.io/hipercow/reference/hipercow_environment.md)
  : Manage environments

## DIDE Cluster support

Functions for use with the DIDE cluster

- [`dide_authenticate()`](https://mrc-ide.github.io/hipercow/reference/dide_authenticate.md)
  : DIDE credentials
- [`dide_check()`](https://mrc-ide.github.io/hipercow/reference/dide_check.md)
  : Check we can use the DIDE cluster
- [`dide_generate_keypair()`](https://mrc-ide.github.io/hipercow/reference/dide_generate_keypair.md)
  [`dide_delete_keypair()`](https://mrc-ide.github.io/hipercow/reference/dide_generate_keypair.md)
  : Generate keypair
- [`dide_path()`](https://mrc-ide.github.io/hipercow/reference/dide_path.md)
  : Describe a path mapping
- [`dide_username()`](https://mrc-ide.github.io/hipercow/reference/dide_username.md)
  : Report DIDE username

## Workers

Use rrq-based workers for faster queues and advanced workflows

- [`hipercow_rrq_controller()`](https://mrc-ide.github.io/hipercow/reference/hipercow_rrq_controller.md)
  : Create an rrq controller
- [`hipercow_rrq_workers_submit()`](https://mrc-ide.github.io/hipercow/reference/hipercow_rrq_workers_submit.md)
  : Submit rrq workers
- [`hipercow_rrq_stop_workers_once_idle()`](https://mrc-ide.github.io/hipercow/reference/hipercow_rrq_stop_workers_once_idle.md)
  : Tell workers to exit once complete

## Utilities

Helpful functions

- [`hipercow_envvars()`](https://mrc-ide.github.io/hipercow/reference/hipercow_envvars.md)
  : Environment variables
- [`hipercow_cluster_info()`](https://mrc-ide.github.io/hipercow/reference/hipercow_cluster_info.md)
  : Describe cluster
- [`hipercow_purge()`](https://mrc-ide.github.io/hipercow/reference/hipercow_purge.md)
  : Purge tasks

## Advanced

Just leave it alone.

- [`hipercow_driver()`](https://mrc-ide.github.io/hipercow/reference/hipercow_driver.md)
  : Create a driver
- [`task_eval()`](https://mrc-ide.github.io/hipercow/reference/task_eval.md)
  : Run a task
- [`task_submit()`](https://mrc-ide.github.io/hipercow/reference/task_submit.md)
  : Submit a task
