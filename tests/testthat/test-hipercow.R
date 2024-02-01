test_that("hipercow", {
  mock_play <- mockery::mock(FALSE)
  mockery::stub(hipercow, "hipercow_speak", mock_play)
  expect_output(hipercow(), "H.+I.+P.+E.+R.+C.+O.+W")
})
