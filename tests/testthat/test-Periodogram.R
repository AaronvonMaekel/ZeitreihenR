test_that("validation works", {
  artest <- AR(1,1,1)
  expect_error(periodogram(artest),"Time series must contain more than one element.")
})

