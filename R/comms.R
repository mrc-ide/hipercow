hipercow_speak_linux <- function(msg) {
  invisible(system(paste0("paplay ", msg), 
                   ignore.stdout = TRUE,
                   ignore.stderr = TRUE, 
                   wait = FALSE))
}

hipercow_speak <- function(msg, platform = Sys.info()["sysname"]) {
  if (platform == "Linux") {
    return(hipercow_speak_linux(msg))
  }
  invisible(audio::play(audio::load.wave(msg)))
}

