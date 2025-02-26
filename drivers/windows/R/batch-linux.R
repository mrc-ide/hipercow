write_batch_task_run_linux <- function(task_id, config, path_root) {

  # Prepare the data to be whisked into the template...

  data <- template_data_task_run(task_id, config, path_root)

  # data contains of note: hostname, date, hipercow_version,
  # hipercow_windows_version, r_version, hipercow_library,
  # renviron_path, task_id, task_id_1 and task_id_2.

  str <- glue_whisker(read_template("task_run.sh"), data)

  # path_to_task_file gets us the path where we need to write the
  # task-specific task_run.sh on the client (not the node). So...

  path <- path_to_task_file(path_root, task_id, SH_RUN)

  # ...path here is the *local* path to task_run.sh, which will start
  # with a drive letter for windows clients, or a local mount point on
  # a linux or mac. Important: we need linux line endings, and
  # writeLines(sep = "\n") still includes \r when run on windows.

  write_linux_lines(str, path)

  # Now calculate the path to this file as the linux node will see it.
  # First convert the local path (which may have a drive letter, or
  # a ~/home/mountpoint for example) unto a UNC

  path_dat <- prepare_path(path, config$shares)

  # unc_to_linux_hpc_mount then converts a UNC path into what the
	# cluster nodes are expecting, for the shares we have currently
	# configured on the nodes. Currently:

  # \\wpia-hn.hpc.dide.ic.ac.uk\share gets mapped as /wpia-hn/share
  # \\qdrive.dide.ic.ac.uk\homes\user gets mapped as /didehomes/user

  linux_path <- unc_to_linux_hpc_mount(path_dat)

  # So, linux_path what you would type to run task_run.sh on the linux node.
  # However...
  # While the job runs, we need to regularly run the linux tool `kinit`,
  # to keep our Kerberos ticket fresh, otherwise we lose access to the
  # multi-user shares (/wpia-hn, /didehomes etc). We have a python
  # script `kwrap.py` which does this, and takes as an argument the path
  # (as the linux nodes sees it), to the run_task.sh file we just made.

  # See https://github.com/mrc-ide/ms-hpc-linux for the implementation
  # of `kwrap.py`. It is copied to the place where all the MS-HPCPack
  # tools live - `/opt/hpcnodemanager`.

  wrap_path <- path_to_task_file(path_root, task_id, SH_WRAP_RUN)

  # Again, path_to_task_file tells us the *local* place where we want to
  # write a file called `wrap_run.sh`. This will run our wrapper, passing
  # the path *as the node sees it* to task_run.sh. (-u requests unbuffered
  # output so we can see a stream of stdout like we can on windows). And
  # again, we want linux line endings here.

  write_linux_lines(
    sprintf("python -u /opt/hpcnodemanager/kwrap.py %s", linux_path),
    wrap_path)

  path
}

write_batch_provision_script_linux <- function(id, config, path_root) {

  # As above... prepare data for whisking.

  data <- template_data_provision_script(id, config, path_root)
  str <- glue_whisker(read_template("provision.sh"), data)

  # Work out the path to the `provision.sh` file, as your *local* machine
  # sees it, as we're going to write that file now. Again, make sure
  # the written 'sh' file has linux line endings.

  path_job <- file.path(path_root, "hipercow", "provision", id)
  path <- file.path(path_job, "provision.sh")
  fs::dir_create(path_job)
  write_linux_lines(str, path)

  # Provisioning jobs should be too short for Kerberos tickets to timeout,
  # but we'll be safe and wrap the script anyway.

  # Use prepare_path to convert the local path to provision.sh
  # into a UNC path

  path_dat <- prepare_path(path, config$shares)

  # And again, convert the UNC path into how the linux cluster node
  # will see that, starting with /wpia-hn or /didehomes perhaps

  linux_path <- unc_to_linux_hpc_mount(path_dat)

  # Here's the local path for `wrap_provision.sh` which we're about to write.

  wrap_path <- file.path(dirname(path), "wrap_provision.sh")

  # And again, write the linux-line-ending-wrapper file, which runs
  # the provision.sh script on the linux node, while also keeping the
  # Kerberos tickets alive.

  write_linux_lines(c(
    sprintf("python -u /opt/hpcnodemanager/kwrap.py %s", linux_path)),
    wrap_path)
  path
}
