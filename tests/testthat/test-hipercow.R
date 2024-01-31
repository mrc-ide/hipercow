test_that("hipercow", {
  mock_speak <- mockery::mock(FALSE)
  mockery::stub(hipercow, "hipercow_speak", mock_speak)
  expect_output(hipercow(), "H.+I.+P.+E.+R.+C.+O.+W")
})
