test_that("validation works", {
  artest <- AR(1,1,10)
  expect_error(sampleACVF(1,1), "Input is not a time series object")
  expect_error(sampleACVF(artest,c(2,2)),"Index is not a singular value")
  expect_error(sampleACVF(artest,"w"),"Index is not of type numeric")
  expect_error(sampleACVF(artest,0.5),"Index is not a integer")
  expect_error(sampleACVF(artest,11),"index out of bounds")
})
