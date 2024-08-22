test_that("plot_timeseries works", {
  matest <- MA(c(1,2), 10, 1)
  expect_error(plot_timeseries(1, matest), "Input is not a time series object")
  expect_error(plot_timeseries(matest, 1), "Input is not a time series object")
})

test_that("plot_SACVF works", {
  matest <- MA(c(1,2), 10, 1)
  expect_error(plot_SACVF(1, FALSE, 5), "Input is not a time series object")
  expect_error(plot_SACVF(matest, "b", 4), "acf not logical")
  expect_error(plot_SACVF(matest, TRUE, "a"), "max_lag not numeric")
  expect_error(plot_SACVF(matest, FALSE, c(2,3,2)), "max_lag not applicable")
})

test_that("plot_spectral_density", {
  matest <- MA(c(1,2), 10, 1)
  expect_error(plot_spectral_density(1,10), "Input is not a time series object")
  expect_error(plot_spectral_density(matest,-5), "n must be a positive integer")
})

test_that("plot_periodogram works", {
  expect_error(plot_periodogram(1), "Input is not a time series object")
})

test_that("plot_ts_overview works", {
  expect_error(plot_ts_overview(1), "Input is not a time series object")
})
