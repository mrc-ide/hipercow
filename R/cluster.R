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
