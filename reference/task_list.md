# List tasks

List hipercow tasks. This is rarely what you want to, but can be useful
in some situations. We get requests for this function periodically, so
here it is! Other, better, functions may be available (see Details).
Please be aware that this function can be quite slow, and do not overuse
it in scripts (please do not, for example, call this repeatedly in a
loop - talk to us if you are tempted to do this).

## Usage

``` r
task_list(
  task_ids = NULL,
  finished_before = NULL,
  in_bundle = NULL,
  with_status = NULL,
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

- in_bundle:

  A character vector of bundle names. Wild cards are supported using
  shell (glob) syntax, rather than regular expression syntax. So use
  `data_*` to match all bundles that start with `data_` (see
  [utils::glob2rx](https://rdrr.io/r/utils/glob2rx.html) for details).
  It is an error if *no* bundles are matched, but not an error if any
  individual pattern does not match.

- with_status:

  A character vector of statuses to match. We only purge tasks that
  match these statuses. Valid statuses to use are `created`,
  `submitted, `running`, `success`, `failure`and`cancelled\`.

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

A character vector. You may want to then pull this vector of ids into a
bundle (e.g.,
[hipercow_bundle_create](https://mrc-ide.github.io/hipercow/reference/hipercow_bundle_create.md)).
The order is arbitrary and does not reflect anything in your tasks.

## Details

Sometimes, better functions are available for you:

- If you want to list tasks in order to delete them, you might prefer
  [`hipercow_purge()`](https://mrc-ide.github.io/hipercow/reference/hipercow_purge.md)

- If you want to list tasks in a bundle, you should use
  [`hipercow_bundle_list()`](https://mrc-ide.github.io/hipercow/reference/hipercow_bundle_list.md)
  to find the bundle and
  [`hipercow_bundle_load()`](https://mrc-ide.github.io/hipercow/reference/hipercow_bundle_load.md)
  to load it (or use other bundle functions)

Once you have listed tasks with `task_list()`
