test_that("hipercow comms works on linux", {
  mock_system <- mockery::mock(FALSE)
  mockery::stub(hipercow_speak_linux, "system2", mock_system)
  hipercow_speak_linux(NULL)
  mockery::expect_called(mock_system, 1)
})


test_that("hipercow comms main call calls linux call", {
  mock_play <- mockery::mock(FALSE)
  mockery::stub(hipercow_speak, "audio::play", mock_play)
  mock_speak_linux <- mockery::mock(FALSE)
  mockery::stub(hipercow_speak, "hipercow_speak_linux", mock_speak_linux)
  hipercow_speak(1, TRUE)
  mockery::expect_called(mock_speak_linux, 1)
  mockery::expect_called(mock_play, 0)
})


test_that("hipercow comms on Windows / Mac", {
  mock_play <- mockery::mock(FALSE)
  mockery::stub(hipercow_speak, "audio::play", mock_play)
  mock_load <- mockery::mock(FALSE)
  mockery::stub(hipercow_speak, "audio::load.wave", mock_load)
  mock_speak_linux <- mockery::mock(FALSE)
  mockery::stub(hipercow_speak, "hipercow_speak_linux", mock_speak_linux)
  hipercow_speak(2, FALSE)
  mockery::expect_called(mock_speak_linux, 0)
  mockery::expect_called(mock_play, 1)
  mockery::expect_called(mock_load, 1)
})
