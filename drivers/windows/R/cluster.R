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


r_versions <- function(platform = "windows") {
  if (!is.list(cache$r_versions)) {
    assign("r_versions", list(), envir = cache)
  }
  if (is.null(cache$r_versions[[platform]])) {
    cache$r_versions[[platform]] <- r_versions_fetch(platform)
  }
  cache$r_versions[[platform]]
}


r_versions_fetch <- function(platform = "windows") {
  credentials <- list(username = "public")
  web_client$new(credentials, platform, login = FALSE)$r_versions()
}


cluster_resources <- function(platform = "windows") {
  if (!is.list(cache$cluster_resources)) {
    assign("cluster_resources", list(), envir = cache)
  }
  if (is.null(cache$cluster_resources[[platform]])) {
    cache$cluster_resources[[platform]] <-
      cluster_resources_fetch(platform)
  }
  cache$cluster_resources[[platform]]
}


cluster_resources_fetch <- function(platform = "windows") {
  credentials <- list(username = "public")
  web_client$new(credentials, platform, login = FALSE)$cluster_resources()
}
