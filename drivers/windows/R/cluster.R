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


r_versions <- function(platform) {
  if (is.null(cache$r_versions)) {
    cache$r_versions <- r_versions_fetch()
  }
  cache$r_versions[[platform]]
}


r_versions_fetch <- function() {
  credentials <- list(username = "public")
  web_client$new(credentials, login = FALSE)$r_versions()
}


cluster_resources <- function(cluster, driver) {
  if (is.null(cache$cluster_resources)) {
    cache$cluster_resources <-
      cluster_resources_fetch(cluster, driver)
  }
  cache$cluster_resources
}


cluster_resources_fetch <- function(cluster, driver) {
  credentials <- list(username = "public")
  web_client$new(credentials, login = FALSE)$cluster_resources(
    "wpia-hn", "hipercow.windows")
}
