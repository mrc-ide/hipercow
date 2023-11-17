cluster_name <- function(name) {
  if (is.null(name)) {
    name <- valid_clusters()[[1L]]
  } else {
    assert_scalar_character(name)
    if (!(name %in% valid_clusters())) {
      alias <- list(
        "fi--dideclusthn" = c("small", "little", "dide", "ide", "dideclusthn"),
        "fi--didemrchnb" = c("big", "mrc", "didemrchnb"))
      alias <- set_names(rep(names(alias), lengths(alias)),
                         unlist(alias, FALSE, FALSE))
      name <- alias[[match_value(tolower(name), names(alias), "name")]]
    }
  }
  name
}


valid_clusters <- function() {
  c("fi--dideclusthn", "fi--didemrchnb", "wpia-hn")
}


valid_templates <- function(cluster) {
  switch(
    cluster,
    "fi--dideclusthn" = c("GeneralNodes", "8Core", "Training"),
    "fi--didemrchnb" = c("GeneralNodes", "12Core", "12and16Core", "16Core",
                         "20Core", "24Core", "32Core", "MEM1024"),
    "wpia-hn" = "AllNodes",
    stop(sprintf("Invalid cluster '%s'", cluster)))
}


valid_cores <- function(cluster) {
  switch(cluster,
         "fi--dideclusthn" = 24,
         "fi--didemrchnb" = 64,
         "wpia-hn" = 32,
         stop(sprintf("Invalid cluster '%s'", cluster)))
}


r_versions <- function() {
  if (is.null(cache$r_versions)) {
    cache$r_versions <- web_client$new("public", login = FALSE)$r_versions()
  }
  cache$r_versions
}
