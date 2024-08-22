test_that("DLA works", {
  matest <- MA(c(1,2), 10, 1)
  expect_error(DLA(1), "Input is not a time series object")
})


test_that("DLA works", {
  matest <- MA(c(1,2), 10, 1)
  expect_error(DL_predictor(1,1,TRUE), "Input is not a time series object")
  expect_error(DL_predictor(matest, 0, FALSE), "Prediction length should be greater or equal to 1")
  expect_error(DL_predictor(matest, c(1,2,3), TRUE), "Entered preditcion length not compatible")
  expect_error(DL_predictor(matest, 2.5, FALSE), "Prediction length is not an integer")
  expect_error(DL_predictor(matest, 2, "c"), "entire_ts has to be logical")
  expect_error(DL_predictor(matest, 3, c(TRUE, FALSE, TRUE)), "Entered value for entire_ts is not compatible")
})
