# Loading required libraries
# muessen wir das in imports umformulieren?? bzw library weg? bzw depends nutzen?
library(methods)

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

# User-defined Constructors

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

AR <- function(ar_params = numeric(0),start_values=numeric(0),n=1,sd=1) {
    p <- length(ar_params)

    stopifnot("Too many start values for requested length of the time series"=p<=n)

    # Initializing time series with zeros
    time_series <- numeric(n)
    
    # Generating white-noise components
    noise <- rnorm(n-p , 0, sd)
    
    # Initializing the first p values
    time_series[1:p] <- start_values
    
    # Generating further values for the AR(p) time series (in case n>p)
    if (n>p) {
        for (t in (p + 1):n) {
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

MA <- function(ma_params = NA_real_,sd=1,n=1) {
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
    if(is.na(object@n)|| length(object@n) == 0){
        errors <- c(errors,"length is not available")
    }
    else{
        if (!is.atomic(object@n)){
        errors <- c(errors,"length is not atomic")
        }
        else if (!is.numeric(object@n)){
            errors <- c(errors,"length is not a numeric value")
        }
        else{
            if (0>object@n){
                errors <- c(errors,"length is not a positive number")
            }
            if (!object@n %% 1 == 0){
                errors <- c(errors,"length is not a integer")
            }
        }
    }
    # Checking standard deviation
    if(!is.na(object@sd) && length(object@sd)!= 0){
    
        if(!is.atomic(object@sd)){
            errors <- c(errors,'standard deviation is not atomic')
        }
        else if (!is.numeric(object@sd)){
            errors <- c(errors,"standard deviation is not a number")
        }
        
        else if (0>object@sd){
            errors <- c(errors,"standard deviation is negative")
        }
       
    }
    
    # Checking data
    if( length(object@data)== 0){
        errors <- c(errors,"data is not available")
    }
    else if (!is.numeric(object@data)){
        errors <- c(errors,"data is not a numeric vector")
    }
    else{
        if(any(is.na(object@data))){
            errors <- c(errors,"data has missing values")
        }
        if(object@n!=length(object@data)){
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
        if(!is.numeric(object@ar_params)){
            errors <- c(errors,"The AR parameters are not numeric")
        }
        
    }
    
    # Checking start values
    if(length(object@start_values)!=0){
        if(!is.numeric(object@start_values)){
            errors <- c(errors,"The start values are not numeric")
        }
        else{
            if( any(is.na(object@start_values))){
                errors <- c(errors,"There are missing start values")
            }
            if( any(object@data[1:length(object@start_values)]!=object@start_values)){
                errors <- c(errors,"The start of the time series differs from the start values")
            }
        }
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
            errors <- c(errors,"there are missing MA parameters")
        }
        if(!is.numeric(object@ma_params)){
            errors <- c(errors,"the MA parameters are not numeric")
        }
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
#'@description Function which can be used to resample a time series. 
#'
#'@param ts_obj A time series, must be a \code{TimeSeries} class.
#'
#'@return The value returned is an \code{TimeSeries} object, having the same length, start values, parameters and standard deviation as the origin time series.
#'
#'@examples 
#'ar_time_series <- AR(ar_params = c(0.3, 0.7), start_values = c(1,2), n = 30, sd = 1)
#'resample(ar_time_series)
#'ma_time_series <- MA(ma_params = c(0.5, 0.7), n = 40, sd = 2)
#'resample(ma_time_series)
#'
#'@export

setGeneric(
    name = "resample",
    def = function(ts_obj) {
        standardGeneric("resample")
    })

setMethod(
    "resample",
    "AR",
    function(ts_obj) {
        AR(ar_params = ts_obj@ar_params, 
           start_values = ts_obj@start_values, 
           n = ts_obj@n, 
           sd = ts_obj@sd)
    })

setMethod(
    "resample",
    "MA",
    function(object) {
        MA(ma_params = object@ma_params,
           n = object@n,
           sd = object@sd)
    })

#' Vector to time series
#'
#'@description Function which transforms a vector into a \code{TimeSeries} object. 
#'
#'@param ts_obj A numeric vector.
#'
#'@return The value returned is an \code{TimeSeries} object, having the same length as the vector.
#'
#'@examples 
#'vec <- 1:10
#'vec_to_ts(vec)
#'
#'@export

setGeneric("vec_to_ts", 
           function(vec) standardGeneric("vec_to_ts"))
setMethod("vec_to_ts",
          "numeric", 
          function(vec) {
              len <- length(vec)
              
              # Use the empirical standard deviation
              sd_vec <- sd(vec)
              
              new("TimeSeries",
                sd = sd_vec,
                n = len,
                data = vec)
            })