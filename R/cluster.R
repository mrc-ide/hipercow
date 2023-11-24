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
    cache$r_versions <- r_versions_fetch()
  }
  cache$r_versions
}


r_versions_fetch <- function() {
  credentials <- list(username = "public", username = "public")
  web_client$new(credentials, login = FALSE)$r_versions()
}
