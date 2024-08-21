#'Sample autocovariance function
#'
#'@description This function takes a \code{TimeSeries} object and computes the sample autocovariance function (SACVF) of the data.
#'
#'@param ts_obj A time series, which must be a \code{TimeSeries} class.
#'@param h Point for which we want the function value of the sample autocovariance function, which must be a length-one numeric vector.
#'
#'@return The return value is a numeric value which can be regarded as estimation of the autocovariance function.
#'
#'@examples
#'ar_ts <- AR(ar_params = 0.5, start_values = 1, n = 50, sd = 1)
#'sampleACVF(ar_ts,2)
#'@references  Peter J. Brockwell, Richard A. Davis (2016) \emph{Introduction to Time-Series Analysis and Forecasting}; Springer Verlag.
#'@export


sampleACVF <- function(ts_obj,h){
              # Validity checks
              stopifnot("Input is not a time series object"=is(ts_obj,"TimeSeries"))
              validObject(ts_obj)
              stopifnot("Index is not a singular value"=length(h)==1)
              stopifnot("Index is not of type numeric"=is.numeric(h))
              stopifnot("Index is not a integer"=(h %% 1 == 0))
              stopifnot("index out of bounds"=abs(h)<ts_obj@n)

              ts <- ts_obj@data
              n <- ts_obj@n
              smpl_mean <- mean(ts)

              summe <-  (1/n) * (ts[(1+abs(h)):n]-smpl_mean) %*% (ts[1:(n-abs(h))]-smpl_mean)
              return(summe[1][1])
          }



