test_that("Innovations_step works", {
  matest <- MA(c(1,2), 10, 1)
  expect_error(innovations_step(2), "Input is not a time series object")
  expect_error(innovations_step(matest, matrix(1,2)), "The matrix has to be a square matrix")
  expect_error(innovations_step(matest, matrix(c(1,2,3,4), nrow=2)), "The matrix has to be a lower triangular matrix")
  expect_error(innovations_step(matest, matrix("a")), "Entry in the matrix is not numeric")
  expect_error(innovations_step(matest, matrix(NA_real_)), "NA entry contained in matrix")
})

test_that("Innovations_predictor", {
  matest <- MA(c(1,2), 10, 1)
  expect_error(innovations_predictor(1,1,TRUE), "Input is not a time series object")
  expect_error(innovations_predictor(matest, 1, "b"), "entire_ts not logical")
  expect_error(innovations_predictor(matest, "g", FALSE), "pred_len not numeric")
  expect_error(innovations_predictor(matest, 0, FALSE), "pred_len must be greater or equal to 1")
  expect_error(innovations_predictor(matest, 2.5, TRUE), "pred_len not applicable")
})
