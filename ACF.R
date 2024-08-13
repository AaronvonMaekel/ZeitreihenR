#'Sample autocovariance function
#'
#'@description Takes a TimeSeries object and computes the sample autocovariance function of the data.
#'
#'@param ts_obj A time series, must be a TimeSeries class.
#'@param h Point for which we want the function value of the sample autocovariance function.
#'
#'@return The return value is a numeric value which can be regarded as an estimation of the autocovariance function.
#'
#'@examples 
#'ar_ts <- AR(ar_params = 0.5, start_values = 1, n = 50, sd = 1)
#'AcF(ar_ts,2)
#'
#'@export

setGeneric("ACF", 
           function(ts_obj,h) standardGeneric("ACF"))
setMethod("ACF",
          "TimeSeries", function(ts_obj,h){
              #validierungen
              validObject(ts_obj)
              stopifnot("Index is not atomic"=is.atomic(h)) 
              stopifnot("Index is not of type numeric"=is.numeric(h))
              stopifnot("Index is not a integer"=(h %% 1 == 0))
              stopifnot("index out of bounds"=abs(h)<ts_obj@n)
              
              ts <- ts_obj@data
              n <- ts_obj@n
              smpl_mean <- mean(ts)
              
              summe <-  (1/n) * (ts[(1+abs(h)):n]-smpl_mean) %*% (ts[1:(n-abs(h))]-smpl_mean)
              return(summe[1][1])
          })

plot(sapply(1:(ma_time_series@n-1),function(h){ACF(ma_time_series,h)}))

