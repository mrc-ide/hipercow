path <- tempfile()
hermod_init(path)
hermod_configure(root = path)


## Run from a network share
unlink(c("hermod.json", "hermod"), recursive = TRUE)

pkgload::load_all("~/Documents/Projects/cluster/hermod")
dide_potato(NULL)

shares <- path_mapping("home",
                       "/home/rfitzjoh/net/home",
                       "//fi--san03.dide.ic.ac.uk/homes/rfitzjoh",
                       "Q:")
dide_potato(NULL)

hermod_init(".")
hermod_configure()






id <- hermod_task_create_explicit(quote(sessionInfo()))

config <- didehpc_config("~/.smbcredentials")
root <- hermod_root(".")
write_batch_task_run(root, config, id)

cl <- web_client$new(config$credentials, login = TRUE)

path_batch <- file.path("//fi--didef3.dide.ic.ac.uk/tmp",
                        "hermod-example/hermod/tasks", id,
                        "batch.bat")
res <- cl$submit(windows_path(path_batch), "hermod!", "GeneralNodes")

hermod_task_status(id)
hermod_task_result(id)
