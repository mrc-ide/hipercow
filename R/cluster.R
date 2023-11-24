cluster_name <- function(name) {
  if (is.null(name)) {
    name <- cluster_name("default")
  } else {
    assert_scalar_character(name)
    if (!(name %in% valid_clusters())) {
      alias <- list(
        "wpia-hn" = c("default", "sk", "new", "windows"))
      alias <- set_names(rep(names(alias), lengths(alias)),
                         unlist(alias, FALSE, FALSE))
      name <- alias[[match_value(tolower(name), names(alias), "name")]]
    }
  }
  name
}


valid_clusters <- function() {
  "wpia-hn"
}


valid_templates <- function(cluster) {
  switch(
    cluster,
    "wpia-hn" = "AllNodes",
    stop(sprintf("Invalid cluster '%s'", cluster)))
}


valid_cores <- function(cluster) {
  switch(cluster,
         "wpia-hn" = 32,
         stop(sprintf("Invalid cluster '%s'", cluster)))
}


r_versions <- function() {
  if (is.null(cache$r_versions)) {
    cache$r_versions <- web_client$new("public", login = FALSE)$r_versions()
  }
  cache$r_versions
}
