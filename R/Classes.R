#'A S4 class to represent a time series
#'
#'@slot sd Standard deviation used for generating the white noise components (length-one numeric vector).
#'@slot n Length of the time series (length-one numeric vector).
#'@slot data Values of the time series (numeric vector with length equal to \code{n}).
#'
#'@export

setClass(
    "TimeSeries",
    slots = list(
        sd = "numeric",
        n = "numeric",
        data = "numeric"
    ),
    prototype = list(sd=1,n=0,data=numeric(0))
)

#'A S4 class to represent an AR time series
#'
#'Child class of \code{TimeSeries}, used for AR(p) time series. \code{p} refers to the length of \code{ar_params}.
#'
#'@slot ar_params Numeric vector containing the parameters for generating an AR time series, length must be equal to the length of \code{start_values}.
#'@slot start_values Numeric vector containing the start values of an AR time series, length must be equal to the length of \code{ar_params}.
#'
#'@export

setClass(
    "AR",
    slots = list(
        ar_params = "numeric",
        start_values= "numeric"
    ),
    contains="TimeSeries",
    prototype = list(ar_params = numeric(0),start_values=numeric(0))
)

#'A S4 class to represent a MA time series
#'
#'Child class of \code{TimeSeries}, used for MA(q) time series. \code{q} refers to the length of \code{ma_params}.
#'
#'@slot ma_params Numeric vector containing the parameters for generating an MA time series.
#'
#'@export

setClass(
    "MA",
    slots = list(
        ma_params = "numeric"
    ),
    contains="TimeSeries",
    prototype = list(ma_params = numeric(0))
)

#'Constructor for AR(p) time series
#'
#'@description Function which can be used to generate an AR(p) time series.
#'
#'@details The white noise components will be drawn independently from a centered normal distribution with standard deviation \code{sd}.
#'
#'@param ar_params Vector of AR parameters, which should have length \code{p}. Parameters must be numeric values.
#'@param start_values Vector containing the \code{p} start values.
#'@param n A length-one numeric vector specifying the length of the time series one wants to obtain. Should be greater or equal to \code{p}.
#'@param sd Standard deviation for generating the white noise components (length-one numeric vector).
#'
#'@return The value returned is an \code{AR} object, having the length and start values specified in beforehand.
#'
#'@examples AR(ar_params = c(0.3, 0.7), start_values = c(1,2), n = 30, sd = 1)
#'
#'@export

AR <- function(ar_params,start_values,n,sd=1) {

    #we have to catch errors in the input arguments here already because the
    #validity checks start after the construction of the object
    #this means that this function is partially validated in the beginning and
    #completely validated in the end.
    #we skip the validation of n,sd being a single value and not a vector,
    #by accessing the first element of these variables instead of the whole variable.
    stopifnot("standard deviation is not available"=(length(sd)>0 && !is.na(sd[1])))
    stopifnot("standard deviation is not a number"=class(sd)=="numeric")
    stopifnot("standard deviation is negative"=sd[1]>=0)
    stopifnot("length is not available"=(length(n)>0 && !is.na(n[1]) ))
    stopifnot("length is not a positive number"=n[1]>0)
    stopifnot("length is not a integer"=n[1] %% 1 == 0)
    stopifnot("AR parameters are not numeric"=class(ar_params)=="numeric")
    stopifnot("start values are not numeric"=class(start_values)=="numeric")
    stopifnot("AR parameters are not available"=(length(ar_params)>0 && !any(is.na(ar_params))))
    stopifnot("start values parameters are not available"=(length(start_values)>0 && !any(is.na(start_values))))

    p <- length(start_values)
    stopifnot("Too many start values for requested length of the time series"=p<=n)

    # Initializing time series with zeros
    time_series <- numeric(n[1])

    # Generating white-noise components
    noise <- rnorm(n[1]-p , 0, sd[1])

    # Initializing the first p values
    time_series[1:p] <- start_values

    # Generating further values for the AR(p) time series (in case n>p)
    if (n[1]>p) {
        for (t in (p + 1):n[1]) {
            time_series[t] <- sum(ar_params * rev(time_series[(t-p):(t-1)])) + noise[t-p]
        }
    }

    new("AR",
        ar_params=ar_params,
        start_values=start_values,
        n=n,
        sd=sd,
        data= time_series)
}

#'Constructor for MA(q) time series
#'
#'@description Function which can be used to generate an MA(q) time series.
#'
#'@details The white noise components will be drawn independently from a centered normal distribution with standard deviation \code{sd}.
#'
#'@param ma_params Vector of MA parameters, which should have length \code{q}. Parameters must be numeric values.
#'@param n A length-one numeric vector specifying the length of the time series one wants to obtain. Should be greater or equal to \code{p}.
#'@param sd Standard deviation for generating the white noise components (length-one numeric vector).
#'
#'@return The value returned is a \code{MA} object, having the length specified in beforehand.
#'
#'@examples MA(ma_params = c(0.5, 0.7), n = 40, sd = 2)
#'
#'@export

MA <- function(ma_params,n,sd=1 ) {
  #we have to catch errors in the input arguments here already because the
  #validity checks start after the construction of the object
  #this means that this function is partially validated in the beginning and
  #completely validated in the end.
  #we skip the validation of n,sd being a single value and not a vector,
  #by accessing the first element of these variables instead of the whole variable.
  stopifnot("standard deviation is not available"=(length(sd)>0 && !is.na(sd[1])))
  stopifnot("standard deviation is not a number"=class(sd)=="numeric")
  stopifnot("standard deviation is negative"=sd[1]>=0)
  stopifnot("length is not available"=(length(n)>0 && !is.na(n[1]) ))
  stopifnot("length is not a positive number"=n[1]>0)
  stopifnot("length is not a integer"=n[1] %% 1 == 0)
  stopifnot("MA parameters are not numeric"=class(ma_params)=="numeric")
  stopifnot("MA parameters are not available"=(length(ma_params)>0 && !any(is.na(ma_params))))




    q <- length(ma_params)

    # Initializing time series with zeros
    time_series <- numeric(n)

    # Generating white-noise components
    noise <- rnorm(n+q, 0, sd)

    # Computing values for the time series
    for (t in 1:n) {
        time_series[t] <- sum(ma_params * rev(noise[t:(t+q-1)])) + noise[t+q]
    }

    new("MA",
        ma_params=ma_params,
        sd=sd,
        data=time_series,
        n=n)
}

# Validations

setValidity("TimeSeries", function(object){
    errors <- character(0)

    # Checking n
    if(is.na(object@n[1])|| length(object@n) == 0){
        errors <- c(errors,"length is not available")
    }
    else{
        if (length(object@n) > 1){
            errors <- c(errors,"length is not a single value")
        }
        else{
            if (0>object@n[1]){
                errors <- c(errors,"length is not a positive number")
            }
            if (!object@n[1] %% 1 == 0){
                errors <- c(errors,"length is not a integer")
            }
        }
    }
    # Checking standard deviation
    if(length(object@sd)>1){
      errors <- c(errors,'standard deviation is not a single value')
    }
    else{
      if(!is.na(object@sd) && length(object@sd)!= 0){
        if (0>object@sd){
            errors <- c(errors,"standard deviation is negative")
        }
      }
    }
    # Checking data
    if( length(object@data)== 0){
        errors <- c(errors,"data is not available")
    }
    else{
        if(any(is.na(object@data))){
            errors <- c(errors,"data has missing values")
        }
        if(object@n[1]!=length(object@data)){
            errors <- c(errors,"data length doesnt correspond to the saved data length")
        }
    }

    if (length(errors)==0){
        return(TRUE)
    }
    else{
        return(errors)
    }
})

setValidity("AR", function(object) {
    errors <- character(0)

    if (length(object@ar_params) != length(object@start_values)) {
        errors <- c(errors,"The number of starting values does not coincide with the amount of coefficients")
    }

    # Checking AR-Parameters
    if(length(object@ar_params)!=0){
        if(any(is.na(object@ar_params))){
            errors <- c(errors,"There are missing AR parameters")
        }
    }
    else{
      errors <- c(errors,"There are no AR parameters")
    }

    # Checking start values
    if(length(object@start_values)!=0){
        if( any(is.na(object@start_values))){
                errors <- c(errors,"There are missing start values")
        }


    }else{
      errors <- c(errors,"There are no Start Values")
    }

    if (length(errors)==0){
        return(TRUE)
    }
    else{
        return(errors)
    }
})

setValidity("MA", function(object) {
    errors <- character(0)

    # Checking MA-Parameters
    if(length(object@ma_params)!=0){
        if(any(is.na(object@ma_params))){
            errors <- c(errors,"There are missing MA parameters")
        }
    }else{
      errors <- c(errors,"There are no MA parameters")
    }

    if (length(errors)==0){
        return(TRUE)
    }
    else{
        return(errors)
    }
})

#'Resample function for a time series
#'
#'
#'@description Function which can be used to resample a time series.
#'
#'@param ts_obj A time series, must be a \code{TimeSeries} class.
#'
#'@return The value returned is an \code{TimeSeries} object, having the same length, start values, parameters and standard deviation as the origin time series.
#'
#'@export
#'@docType methods
#'@rdname resample-methods
#'
#'@examples
#'ar_time_series <- AR(ar_params = c(0.3, 0.7), start_values = c(1,2), n = 30, sd = 1)
#'resample(ar_time_series)
#'ma_time_series <- MA(ma_params = c(0.5, 0.7), n = 40, sd = 2)
#'resample(ma_time_series)
setGeneric(
    name = "resample",
    def = function(ts_obj) {
        standardGeneric("resample")
    })


#' @rdname resample-methods
#' @aliases resample,AR
setMethod(
    "resample",
    "AR",
    function(ts_obj) {
        AR(ar_params = ts_obj@ar_params,
           start_values = ts_obj@start_values,
           n = ts_obj@n,
           sd = ts_obj@sd)
    })

#' @rdname resample-methods
#' @aliases resample,MA
setMethod(
    "resample",
    "MA",
    function(ts_obj) {
        MA(ma_params = ts_obj@ma_params,
           n = ts_obj@n,
           sd = ts_obj@sd)
    })


setAs("numeric", "TimeSeries", function(from) {
  len <- length(from)

  # Use the empirical standard deviation
  sd_vec <- sd(from)

  new("TimeSeries",
      sd = sd_vec,
      n = len,
      data = from)})

setMethod("show", "TimeSeries",
          function(object){
            validObject(object)
            header <- "Time series"

            if (is(object,"AR")){
              header <- paste0("AR(",length(object@ar_params),")-",header)
            }
            else if (is(object,"MA")){
              header <- paste0("MA(",length(object@ma_params),")-",header)
            }
            cat(header, "with length",object@n,"\n")
            print(object@data)

            })


