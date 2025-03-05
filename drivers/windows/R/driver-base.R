dide_driver_base <- function() {
  hipercow::hipercow_driver(

    # These are common between linux and windows drivers

    status = windows_status,
    info = windows_info,
    log = windows_log,
    result = windows_result,
    cancel = windows_cancel,
    provision_run = dide_provision_run,
    provision_list = windows_provision_list,
    provision_compare = windows_provision_compare,
    keypair = windows_keypair,
    cluster_info = windows_cluster_info,
    default_envvars = DEFAULT_ENVVARS,

    # These are defined by the linux and windows drivers

    configure = NULL,
    submit = NULL,
    check_hello = NULL)
}
