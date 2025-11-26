# Purge tasks

Purge (delete) hipercow tasks. This is a destructive operation that
cannot be undone and can have unintended consequences! However, if you
are running short of space and don't want to just delete everything and
start again (which is our general recommendation), this function
provides a mechanism for cleaning up tasks that you no longer need.

## Usage

``` r
hipercow_purge(
  task_ids = NULL,
  finished_before = NULL,
  in_bundle = NULL,
  with_status = NULL,
  dry_run = FALSE,
  root = NULL
)
```

## Arguments

- task_ids:

  A character vector of task identifiers. Typically if you provide this
  you will not provide any other filters.

- finished_before:

  A date, time, or [difftime](https://rdrr.io/r/base/difftime.html)
  object representing the time or time ago that a task finished (here,
  the job might have finished for any reason; successfully or
  unsuccessfully unless you also provide the `with_status` argument).
  Everything prior to this will be deleted.

- in_bundle:

  A character vector of bundle names. Wild cards are supported using
  shell (glob) syntax, rather than regular expression syntax. So use
  `data_*` to match all bundles that start with `data_` (see
  [utils::glob2rx](https://rdrr.io/r/utils/glob2rx.html) for details).
  It is an error if *no* bundles are matched, but not an error if any
  individual pattern does not match.

- with_status:

  A character vector of statuses to match. We only purge tasks that
  match these statuses. Valid statuses to use are `created`, `success`,
  `failure` and `cancelled` (note you cannot select tasks with status of
  `submitted` or `running`; use
  [task_cancel](https://mrc-ide.github.io/hipercow/reference/task_cancel.md)
  for these first).

- dry_run:

  If TRUE, report what would have been done, but no changes will be
  made.

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

A character vector of deleted identifiers, invisibly.

## Details

Most of the arguments describe *filters* over your tasks. We delete the
intersection of these filters (not the union), and you must provide at
least one filter. So to delete all tasks that were created more than a
week ago you could write:

    hipercow_purge(created_before = as.difftime(1, units = "weeks"))

but to restrict this to only tasks that have *also failed* you could
write

    hipercow_purge(created_before = "1 week", with_status = "failed")

## Consequences of deletion

A non-exhaustive list:

- If you delete a task that is part of a
  [task_retry](https://mrc-ide.github.io/hipercow/reference/task_retry.md)
  chain, then all tasks (both upstream and downstream in that chain) are
  deleted

- Once we support task dependencies (mrc-4797), deleting tasks will mark
  any not-yet-run dependent task as impossible, or perhaps delete it
  too, or prevent you from deleting the task; we've not decided yet

- You may have a bundle that references a task that you delete, in which
  case the bundle will not behave as expected. As a result we delete all
  bundles that reference a deleted task

- Deleted bundles or deleted tasks that you hold identifiers to before
  deletion will not behave as expected, with tasks reported missing.
  Restarting your session is probably the safest thing to do after
  purging.

- We can't prevent race conditions, so if you are purging tasks at the
  same time you are also retrying tasks that you will purge, you'll
  create tasks that we might not want to allow, and these tasks will
  fail in peculiar ways.

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper

# Here are some tasks that have finished running:
bundle <- task_create_bulk_expr(sqrt(x), data.frame(x = 1:5),
                                bundle_name = "mybundle")
#> ✔ Submitted 5 tasks using 'example'
#> ✔ Created bundle 'mybundle' with 5 tasks
hipercow_bundle_wait(bundle)
#> [1] TRUE

# Purge all tasks contained in any bundle starting with "my":
hipercow_purge(in_bundle = "my*")
#> ℹ Purging 5 tasks
#> ℹ Deleting 1 task bundle

cleanup()
#> ℹ Cleaning up example
```
