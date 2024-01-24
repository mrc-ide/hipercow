hipercow_speak_linux <- function(msg) {
  system2("paplay", msg, stdout = FALSE, stderr = FALSE, wait = FALSE)
}

hipercow_speak <- function(msg_code, on_linux = is_linux()) {
  msg <- hipercow_file(sprintf("comms/moo%s.wav", msg_code))
  if (on_linux) {
    return(hipercow_speak_linux(msg))
  } else {
    try(audio::play(audio::load.wave(msg)))
  }
  invisible()
}
