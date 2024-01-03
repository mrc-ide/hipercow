#!/usr/bin/env Rscript
root <- here::here()
img <- png::readPNG(file.path(root, "man/figures/logo-sm.png"))
col <- rgb(img[, , 1], img[, , 2], img[, , 3])
options(cli.num_colors = 256)
ansi <- lapply(col, function(x) attributes(cli::make_ansi_style(x)))
ansi_open <- vapply(ansi, function(x) x[["_styles"]][[1]]$open, "")
ansi_close <- unique(vapply(ansi, function(x) x[["_styles"]][[1]]$close, ""))
env <- new.env(parent = emptyenv())
env$logo_cols <- list(open = ansi_open, close = ansi_close, dim = dim(img)[1:2])
save(list = ls(env), file = file.path(root, "R/sysdata.rda"),
     envir = env, version = 2)
