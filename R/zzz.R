cache <- new.env(parent = emptyenv())

##' The default set of environment variables used for all tasks.
##'
##' These are chosen to provide the best general experience. The default
##' variables may be overriden globally by setting the hipercow.default_envvars
##' option, or on a per-task and variable basis by assigning a different value
##' to the environment variable in question.
##'
##' @keywords internal
DEFAULT_ENVVARS <- hipercow_envvars("R_GC_MEM_GROW" = "3")
