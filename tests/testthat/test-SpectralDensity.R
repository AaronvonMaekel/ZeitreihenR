test_that("error handling works", {
  expect_error(spectral_density(1),"Input is not a time series object")
  expect_error(spectral_density(as(c(1),"TimeSeries")),"Spectral Density not available")
  artest <- AR(1,1,10)
  spec_f <- spectral_density(artest)
  expect_error(spec_f(0.7))
  expect_error(spec_f(-0.1))
})
