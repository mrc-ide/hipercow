get_web_client <- function() {
  if (is.null(cache$web_client)) {
    cache$web_client <- web_client$new(windows_credentials(), login = FALSE)
  }
  cache$web_client
}

web_client <- R6::R6Class(
  "web_client",
  cloneable = FALSE,

  public = list(
    initialize = function(credentials, cluster_default = NULL, login = FALSE,
                          client = NULL) {
      private$client <- client %||% api_client$new(credentials)
      private$cluster <- cluster_name(cluster_default)
      if (login) {
        self$login()
      }
    },

    login = function(refresh = TRUE) {
      private$client$login(refresh = refresh)
    },

    logout = function() {
      private$client$logout()
    },

    logged_in = function() {
      private$client$logged_in()
    },

    check_access = function(cluster = NULL) {
      client_check(cluster %||% private$cluster, self$headnodes())
    },

    submit = function(path, name, template, cluster = NULL,
                      resource_type = "Cores", resource_count = 1,
                      depends_on = NULL) {
      data <- client_body_submit(
        path, name, template, cluster %||% private$cluster,
        resource_type, resource_count, depends_on)
      r <- private$client$POST("/submit_1.php", data)
      client_parse_submit(httr_text(r), 1L)
    },

    cancel = function(dide_id, cluster = NULL) {
      data <- client_body_cancel(dide_id, cluster %||% private$cluster)
      r <- private$client$POST("/cancel.php", data)
      client_parse_cancel(httr_text(r))
    },

    log = function(dide_id, cluster = NULL) {
      data <- client_body_log(dide_id, cluster %||% private$cluster)
      r <- private$client$POST("/showjobfail.php", data)
      client_parse_log(httr_text(r))
    },

    status_user = function(state = "*", cluster = NULL) {
      data <- client_body_status(state, private$client$username(),
                                 cluster %||% private$cluster)
      r <- private$client$POST("/_listalljobs.php", data)
      client_parse_status(httr_text(r))
    },

    status_job = function(dide_id, cluster = NULL) {
      pars <- list(scheduler = cluster %||% private$cluster,
                   jobid = dide_id)
      r <- private$client$GET("/api/v1/get_job_status/", query = pars)
      status_map(httr_text(r))
    },

    load_node = function(cluster = NULL) {
      cluster <- cluster %||% private$cluster
      data <- list(cluster = encode64(cluster %||% private$cluster),
                   hpcfunc = "shownodes",
                   cluster_no = as.character(match(cluster, valid_clusters())))
      r <- private$client$POST("/shownodes.php", data)
      client_parse_load_cluster(httr_text(r), cluster)
    },

    load_overall = function() {
      dat <- lapply(self$headnodes(), self$load_node)
      client_parse_load_overall(dat)
    },

    load_show = function(cluster = NULL, nodes = TRUE) {
      if (isTRUE(cluster)) {
        print(self$load_overall())
      } else {
        print(self$load_node(cluster %||% self$cluster),
              nodes = nodes)
      }
    },

    headnodes = function(forget = FALSE) {
      if (forget || is.null(private$headnodes_)) {
        data <- list(user = encode64(""))
        r <- private$client$POST("/_listheadnodes.php", data)
        private$headnodes_ <- client_parse_headnodes(httr_text(r))
      }
      private$headnodes_
    },

    r_versions = function() {
      r <- private$client$GET("/api/v1/cluster_software/", public = TRUE)
      client_parse_r_versions(httr_text(r))
    },

    api_client = function() {
      private$client
    }
  ),

  private = list(
    client = NULL,
    cluster = NULL,
    headnodes_ = NULL
  ))

## This is code directly copied over from didehpc; we may want to
## change this pretty fundamentally in future, but because we'll also
## look at the portal itself over the next year or so, we'll pause on
## that now. This was imported from didehpc version 0.3.22 and works
## well in practice.
api_client <- R6::R6Class(
  "api_client",
  cloneable = FALSE,

  public = list(
    initialize = function(credentials) {
      private$credentials <- credentials
    },

    username = function() {
      private$credentials$username
    },

    GET = function(path, ...) {
      self$request(httr::GET, path, ...)
    },

    POST = function(path, body, ...) {
      self$request(httr::POST, path, body = body, ...,
                   httr::accept("text/plain"), encode = "form")
    },

    request = function(verb, path, ..., public = FALSE) {
      self$login(public)
      url <- paste0(private$url, path)
      r <- verb(url, ...)
      status <- httr::status_code(r)
      if (status %in% c(401, 403)) {
        cli::cli_alert_warning(
          "Trying to login again, previous session likely expired")
        self$login(public, refresh = TRUE)
        r <- verb(url, ...)
      }
      httr::stop_for_status(r)
      r
    },

    login = function(public = FALSE, refresh = FALSE) {
      if (public && !refresh) {
        return()
      }
      if (refresh || !private$has_logged_in) {
        api_client_login(private$credentials$username,
                         private$credentials$password)
        private$has_logged_in <- TRUE
      }
    },

    logout = function() {
      private$has_logged_in <- FALSE
      self$GET("/logout.php", public = TRUE)
      invisible()
    },

    logged_in = function() {
      if (!private$has_logged_in) {
        return(FALSE)
      }
      r <- self$POST("/_listheadnodes.php", list(user = encode64("")))
      httr::status_code(r) < 300
    }
  ),

  private = list(
    url = "https://mrcdata.dide.ic.ac.uk/hpc",
    credentials = NULL,
    has_logged_in = FALSE
  ))


api_client_login <- function(username, password) {
  data <- list(us = encode64(username),
               pw = encode64(password),
               hpcfunc = encode64("login"))
  r <- httr::POST("https://mrcdata.dide.ic.ac.uk/hpc/index.php",
                  body = data, encode = "form")
  httr::stop_for_status(r)
  ## We get this where DIDE username/password ok but access is not
  ## allowed
  if (grepl("You don't seem to have any HPC access", httr_text(r))) {
    stop("You do not have HPC access - please contact Wes")
  }
}


client_body_submit <- function(path, name, template, cluster,
                               resource_type, resource_count, depends_on) {
  ## TODO: this clearly used to allow batch submission of several jobs
  ## at once, and we should consider re-allowing that. It looks like
  ## the issue is we can't easily get the names sent as a vector? Or
  ## is that allowed?
  assert_scalar_character(path)
  if (!grepl("^\\\\\\\\", path)) {
    stop("All paths must be Windows network paths")
  }
  path_call <- paste("call", shQuote(path, "cmd"))

  name <- name %||% ""
  assert_scalar_character(name)

  deps <- paste0(depends_on, collapse = ",")

  workdir <- ""
  stderr <- ""
  stdout <- ""
  list(
    cluster = encode64(cluster),
    template = encode64(template),
    rc = encode64(as.character(resource_count)),
    rt = encode64(resource_type),
    jn = encode64(name),
    wd = encode64(workdir),
    se = encode64(stderr),
    so = encode64(stdout),
    jobs = encode64(path_call),
    dep = encode64(deps),
    hpcfunc = "submit")
}


client_body_cancel <- function(dide_id, cluster) {
  if (length(dide_id) == 0L) {
    stop("Need at least one task to cancel")
  }
  jobs <- set_names(as.list(dide_id), paste0("c", dide_id))
  c(list(cluster = encode64(cluster),
         hpcfunc = encode64("cancel")),
    jobs)
}


client_body_log <- function(dide_id, cluster) {
  assert_scalar_character(dide_id)
  list(hpcfunc = "showfail",
       cluster = encode64(cluster),
       id = dide_id)
}


client_body_status <- function(state, username, cluster) {
  valid <- c("*", "Running", "Finished", "Queued", "Failed", "Cancelled")
  state <- match_value(state, valid)
  list(user = encode64(username),
       scheduler = encode64(cluster),
       state = encode64(state),
       jobs = encode64(as.character(-1)))
}


client_parse_status <- function(txt) {
  cols <- c("dide_id", "name", "status", "resources", "user",
            "time_start", "time_submit", "time_end", "template")
  ## Id Name State Resources User StartTime SubmitTime EndTime JobTemplate
  if (nzchar(txt)) {
    res <- strsplit(strsplit(txt, "\n")[[1]], "\t")
    len <- lengths(res)
    if (any(len != length(cols))) {
      stop("Parse error; unexpected output from server")
    }
    res <- as.data.frame(do.call(rbind, res), stringsAsFactors = FALSE)
  } else {
    res <- as.data.frame(matrix(character(0), 0, length(cols)),
                         stringsAsFactors = FALSE)
  }
  names(res) <- cols

  ## Some type switches:
  res$dide_id <- res$dide_id
  res$name <- trimws(res$name)
  res$name[!nzchar(res$name)] <- NA_character_
  res$user <- sub("^DIDE\\\\", "", res$user)
  res$status <- status_map(res$status)
  res$time_start <- dide_time_parse(res$time_start)
  res$time_end <- dide_time_parse(res$time_end)
  res$time_submit <- dide_time_parse(res$time_submit)
  res
}


client_parse_log <- function(txt) {
  xml <- xml2::read_html(txt)
  value <- xml2::xml_attr(xml2::xml_find_first(xml, '//input[@id="res"]'),
                          "value")
  if (nzchar(value)) {
    value <- trimws(sub("^Output\\s*:\\s*?\n+", "", decode64(value)))
    strsplit(value, "\n")[[1]]
  } else {
    NULL
  }
}


client_parse_r_versions <- function(txt) {
  dat <- from_json(txt)
  dat_r <- dat$software[vcapply(dat$software, "[[", "name") == "R"]
  numeric_version(vcapply(dat_r, "[[", "version"))
}


client_parse_headnodes <- function(txt) {
  dat <- strsplit(txt, "\n")[[1]]
  stopifnot(all(grepl("^(fi--|wpia-)", dat)))
  setdiff(dat, "fi--didelxhn")
}


client_parse_submit <- function(txt, n) {
  txt <- strsplit(txt, "\n")[[1]]
  re <- "^Job has been submitted. ID: +([0-9]+)\\.$"
  i <- grepl(re, txt)

  extra <- txt[!i]
  if (length(extra) > 0L) {
    message("Discarding additional response from server:\n",
            paste(extra, collapse = "\n"))
  }

  nok <- sum(i)
  if (nok > 0L) {
    if (nok != n) {
      ## Hopefully never triggers
      stop("Unexpected response length from server")
    }
    sub(re, "\\1", txt[i])
  } else {
    ## TODO: Detect this and hit login and try again?
    stop("Job submission has likely failed; could be a login error")
  }
}


client_parse_cancel <- function(txt) {
  d <- strsplit(txt, "\n")[[1]]
  d <- strsplit(d[nzchar(d)], "\t")
  set_names(vcapply(d, "[[", 2L), vcapply(d, "[[", 1L))
}

client_parse_load_cluster <- function(txt, cluster) {
  cluster <- tolower(cluster)
  txt <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  re <- "^([^ ]+) +- +([0-9]+) +([^ ]+) *(.*)$"
  d <- txt[-seq_len(2)]
  d <- d[nzchar(d)]
  node <- sub(re, "\\1", d)
  core <- as.integer(sub(re, "\\2", d)) + 1L
  status <- sub(re, "\\3", d)
  rest <- sub(re, "\\4", d)
  task_id <- rep(NA_character_, length(d))
  i <- nchar(rest) > 0L
  task_id[i] <- sub("^([0-9]+).*", "\\1", rest[i])
  res <- data.frame(node = tolower(node), core = core, status = status,
                    dide_id = task_id, stringsAsFactors = FALSE)
  res <- res[res$node != cluster, ]
  res <- res[order(res$node), ]
  free <- tapply(res$status == "Idle", res$node, sum)
  total <- tapply(res$node, res$node, length)
  used <- total - free
  percent_used <- round(used / total * 100)

  summary <- data.frame(name = names(free),
                        free = unname(free),
                        used = unname(used),
                        total = unname(total),
                        percent_used = unname(percent_used),
                        stringsAsFactors = FALSE)

  overall <- list(name = cluster,
                  free = sum(free),
                  used = sum(total) - sum(free),
                  total = sum(total),
                  percent_used = round((1 - sum(free) / sum(total)) * 100))

  ret <- list(cluster = cluster,
              detail = res,
              summary = summary,
              overall = overall)
  class(ret) <- "dide_clusterload"
  ret
}


client_parse_load_overall <- function(dat) {
  summary <- do.call("rbind", lapply(dat, function(x) {
    as.data.frame(x$overall, stringsAsFactors = FALSE)
  }))
  overall <- list(name = "didehpc",
                  free = sum(summary$free),
                  used = sum(summary$used),
                  total = sum(summary$total))
  overall$percent_used <- round(100 * overall$used / overall$total)
  ret <- list(cluster = "didehpc",
              detail = NULL,
              summary = summary,
              overall = overall)
  class(ret) <- "dide_clusterload"
  ret
}


client_check <- function(cluster, valid) {
  if (!(cluster %in% valid)) {
    if (length(valid) == 0L) {
      stop("You do not have access to any cluster")
    } else if (length(valid) == 1L) {
      stop(sprintf("You do not have access to '%s'; try '%s'", cluster, valid))
    } else {
      stop(sprintf("You do not have access to '%s'; try one of %s",
                   cluster, paste(squote(valid), collapse = ", ")))
    }
  }
}


status_map <- function(x) {
  map <- c(Running = "RUNNING",
           Finished = "COMPLETE",
           Queued = "PENDING",
           Failed = "ERROR",
           Canceled = "CANCELLED",
           Cancelled = "CANCELLED")
  ret <- unname(map[x])
  ret[is.na(ret)] <- "MISSING"
  ret
}


dide_time_parse <- function(x) {
  ## YYYYMMDDHHMMSS
  ## 20151109170805
  as.POSIXct(strptime(x, "%Y%m%d%H%M%S"))
}


##' @export
format.dide_clusterload <- function(x, ..., nodes = TRUE) {
  f <- function(name) {
    vals <- c(x$overall[[name]], x$summary[[name]])
    if (name == "percent_used") {
      name <- "% used"
      vals <- paste0(vals, "%")
    }
    format(c(name, vals), justify = "right")
  }
  m <- cbind(f("name"), f("free"), f("used"), f("total"), f("percent_used"))

  ## Header:
  mh <- vcapply(m[1, ], crayon::bold)

  ## Divider:
  md <- vcapply(nchar(m[1, ]), strrep, x = "-")

  ## Summary
  if (nodes) {
    ms <- m[-seq_len(2), , drop = FALSE]
    col <- cluster_load_cols(x$summary$used / x$summary$total)
    ms[, 1] <- crayon::blue(ms[, 1])
    ms[, -1] <- t(vapply(seq_along(col),
                         function(i) crayon::make_style(col[[i]])(ms[i, -1]),
                         character(ncol(m) - 1L)))
    ms <- rbind(ms, md, deparse.level = 0)
  } else {
    ms <- NULL
  }

  ## Overall
  mo <- m[2, ]
  col <- cluster_load_cols(x$overall$used / x$overall$total)
  mo[1] <- crayon::make_style("blue")$bold(mo[1])
  mo[-1] <- vcapply(mo[-1], crayon::make_style(col)$bold)

  mm <- rbind(mh, md, ms, mo, deparse.level = 0)

  apply(mm, 1, paste, collapse = " ")
}


##' @export
print.dide_clusterload <- function(x, ...) {
  cat(paste0(format(x, ...), "\n", collapse = ""))
  invisible(x)
}


cluster_load_cols <- function(p, max = 1) {
  cols <- c("#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#B10026")
  p[is.nan(p)] <- 0
  ret <- grDevices::colorRamp(cols)(p / max)
  grDevices::rgb(ret[, 1], ret[, 2], ret[, 3], maxColorValue = 255)
}
