test_that("AR constructor validation works,", {
  expect_error(AR(1,1,1,sd=numeric(0)),"standard deviation is not available")
  expect_error(AR(1,1,1,sd=NA_real_),"standard deviation is not available")
  expect_error(AR(1,1,1,sd="w"),"standard deviation is not a number")
  expect_error(AR(1,1,1,-1),"standard deviation is negative")
  expect_error(AR(1,1,NA_real_,1),"length is not available")
  expect_error(AR(1,1,-1,1),"length is not a positive number")
  expect_error(AR(1,1,1.7,1),"length is not a integer")
  expect_error(AR("w",1,1,1),"AR parameters are not numeric")
  expect_error(AR(1,"w",1,1),"start values are not numeric")
  expect_error(AR(numeric(0),1,1,1),"AR parameters are not available")
  expect_error(AR(1,numeric(0),1,1),"start values parameters are not available")
  expect_error(AR(1,c(2,2),1,1),"Too many start values for requested length of the time series")


})
test_that("MA constructor validation works,", {
  expect_error(MA(1,1,sd=numeric(0)),"standard deviation is not available")
  expect_error(MA(1,1,sd=NA_real_),"standard deviation is not available")
  expect_error(MA(1,1,sd="w"),"standard deviation is not a number")
  expect_error(MA(1,1,-1),"standard deviation is negative")
  expect_error(MA(1,NA_real_,1),"length is not available")
  expect_error(MA(1,-1,1),"length is not a positive number")
  expect_error(MA(1,1.7,1),"length is not a integer")
  expect_error(MA("w",1,1),"MA parameters are not numeric")
  expect_error(MA(numeric(0),1,1),"MA parameters are not available")


})
test_that("AR  Fourier Example works,", {
  artest <- AR(c(1,1),c(1,1),6,0)
  expect_equal(c(1,1,2,3,5,8), artest@data)
})


test_that("conversion works,", {

  test <- as(c(1,1,2,3,5,8),"TimeSeries")
  expect_equal(class(test)[1],"TimeSeries")
})

test_that("TimeSeries validity works,", {
  expect_error(new("TimeSeries",sd=1,n=10,data=numeric(0)),r"(invalid class "TimeSeries" object: data is not available)")

  expect_error(new("TimeSeries",sd=1,n=1,data=c(NA_real_)),r"(invalid class "TimeSeries" object: data has missing values)")
  expect_error(new("TimeSeries",sd=1,n=10,data=c(1)),r"(invalid class "TimeSeries" object: data length doesnt correspond to the saved data length)")

  expect_error(new("TimeSeries",sd=-1,n=1,data=c(1)),r"(invalid class "TimeSeries" object: standard deviation is negative)")
  expect_error(new("TimeSeries",sd=c(1,1),n=1,data=c(1)),r"(invalid class "TimeSeries" object: standard deviation is not a single value)")

  expect_error(new("TimeSeries",sd=1,n=numeric(0),data=numeric(0)),r"(invalid class "TimeSeries" object: 1: length is not available
invalid class "TimeSeries" object: 2: data is not available)")
  expect_error(new("TimeSeries",sd=1,n=c(1,2),data=c(1)),r"(invalid class "TimeSeries" object: length is not a single value)")
  expect_error(new("TimeSeries",sd=1,n=-10,data=c(1)),r"(invalid class "TimeSeries" object: 1: length is not a positive number
invalid class "TimeSeries" object: 2: data length doesnt correspond to the saved data length)")
  expect_error(new("TimeSeries",sd=1,n=2.3,data=c(1)),r"(invalid class "TimeSeries" object: 1: length is not a integer
invalid class "TimeSeries" object: 2: data length doesnt correspond to the saved data length)")



  })

test_that("AR validity works,", {
  expect_error(new("AR",sd=1,n=1,data=c(1),ar_params=numeric(0),start_values=c(1)),r"(invalid class "AR" object: 1: The number of starting values does not coincide with the amount of coefficients
invalid class "AR" object: 2: There are no AR parameters)")
  expect_error(new("AR",sd=1,n=1,data=c(1),ar_params=c(NA_real_),start_values=c(1)),r"(invalid class "AR" object: There are missing AR parameters)")
  expect_error(new("AR",sd=1,n=1,data=c(1),ar_params=c(1),start_values=numeric(0)),r"(invalid class "AR" object: 1: The number of starting values does not coincide with the amount of coefficients
invalid class "AR" object: 2: There are no Start Values)")
  expect_error(new("AR",sd=1,n=1,data=c(1),ar_params=c(1),start_values=c(NA_real_)),r"(invalid class "AR" object: There are missing start values)")
})

test_that("MA validity works,", {
  expect_error(new("MA",sd=1,n=1,data=c(1),ma_params=numeric(0)),r"(invalid class "MA" object: There are no MA parameters)")
  expect_error(new("MA",sd=1,n=1,data=c(1),ma_params=c(NA_real_)),r"(invalid class "MA" object: There are missing MA parameters)")
  })

