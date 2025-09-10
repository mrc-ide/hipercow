dide_generate_keypair <- function(update = FALSE, call = NULL) {
  username <- dide_username()
  if (!update) {
    pubkey <- tryCatch(keyring::key_get("hipercow/dide/pubkey",
                                        username = username),
                       error = function(e) NULL)
    if (!is.null(pubkey)) {
      cli::cli_alert_warning(
        "Not generating a new keypair as existing keypair detected")
      cli::cli_alert_info(paste(
        "If you have deleted your private key and want to regenerate it",
        "you will need to run 'dide_generate_keypair(update = TRUE)'"))
      return(invisible())
    }
  }

  path <- dide_keypair_local_path(username)
  if (!is.null(path)) {
    pub <- dide_generate_keypair_locally(path)
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


dide_generate_keypair_locally <- function(dest) {
  key <- openssl::rsa_keygen()
  fs::dir_create(dirname(dest$private))
  openssl::write_pem(key, dest$private)
  cli::cli_alert_success("Created new private key at '{dest$private}'")
  pub <- as.list(key)$pubkey
  openssl::write_ssh(pub, dest$public)
  cli::cli_alert_success("Created new public key at '{dest$public}'")
  pub
}


dide_delete_keypair <- function() {
  username <- dide_username()
  keyring::key_delete("hipercow/dide/pubkey", username = username)
  cli::cli_alert_success("Deleted keypair from your keyring (if it existed)")
  path <- dide_keypair_local_path(username)
  if (!is.null(path)) {
    unlink(c(path$public, path$private))
    cli::cli_alert_success(paste(
      "Deleted on-disk copies of the keys from your home network drive",
      "(if they existed)"))
  }
  invisible()
}


dide_keypair_local_path <- function(username) {
  share <- sprintf("//qdrive.dide.ic.ac.uk/homes/%s", username)
  path_share <- dide_locally_resolve_unc_path(share)
  if (is.null(path_share) || !file.exists(path_share)) {
    return(NULL)
  }
  dide_keypair_path(path_share)
}


dide_keypair_path <- function(path) {
  list(private = file.path(path, ".hipercow/key"),
       public = file.path(path, ".hipercow/key.pub"))
}
