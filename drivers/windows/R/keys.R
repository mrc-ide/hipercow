windows_generate_keypair <- function(update = FALSE, call = NULL) {
  username <- windows_username()
  if (!update) {
    pubkey <- tryCatch(keyring::key_get("hipercow/dide/pubkey",
                                        username = username),
                       error = function(e) NULL)
    if (!is.null(pubkey)) {
      cli::cli_alert_warning(
        "Not generating a new keypair as existing keypair detected")
      cli::cli_alert_info(paste(
        "If you have deleted your private key and want to regenerate it",
        "you will need to run 'windows_generate_keypair(update = TRUE)'"))
      return(invisible())
    }
  }
  share <- sprintf("//qdrive.dide.ic.ac.uk/homes/%s", username)
  share_local <- dide_locally_resolve_unc_path(share)
  if (!is.null(share_local) && file.exists(share_local)) {
    pub <- windows_generate_keypair_locally(share_local)
  } else {
    ## here we'll run a small hipercow job, we'll need to check we're
    ## in a hipercow root etc first so that the error message is nice;
    ## but for now we'll just error; see mrc-4932 for details.
    cli::cli_abort(
      c("Can't generate a keypair as I failed to find your home drive",
        i = "We will support this soon, can you let us know please?"),
      call = call)
  }
  keyring::key_set_with_value("hipercow/dide/pubkey",
                              username = username,
                              password = openssl::write_ssh(pub))
  cli::cli_alert_success("Saved public key into your keychain")
  invisible()
}


windows_generate_keypair_locally <- function(path_share) {
  key <- openssl::rsa_keygen()
  dest <- file.path(path_share, ".hipercow/key")
  fs::dir_create(dirname(dest))
  openssl::write_pem(key, dest)
  cli::cli_alert_success("Created new private key at '{dest}'")
  as.list(key)$pubkey
}
