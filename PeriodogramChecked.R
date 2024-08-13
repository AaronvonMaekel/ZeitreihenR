#'Periodogram for a time series
#'
#'@description Takes a \code{TimeSeries} object and computes the periodogram of the time series.
#'
#'@param ts_obj A time series, must be a \code{TimeSeries} class.
#'
#'@return The return value is a list with values for the frequency and the spectral density.
#'
#'@examples 
#'ar_time_series <- AR(start_values = c(1, 1), ar_params = c(0.6, 0.6), sd = 1, n = 100)
#'periodogram(ar_time_series)
#'
#'@export

setGeneric("periodogram", 
           function(ts_obj) standardGeneric("periodogram"))

setMethod("periodogram",
          "TimeSeries",
          function(ts_obj){
            # Validity
            validObject(ts_obj)
            
            data <- ts_obj@data
            len <- ts_obj@n
            
            # Timeseries must contain more than one element
            stopifnot("Die Zeitreihe muss mehr als ein Element enthalten."=len>1)
            
            # Initializing periodogram 
            spec <- numeric(len)
            
            # Frequency index
            freq_index <- seq(-floor((len-1)/2),floor(len/2))
            
            # Calculating the periodogram
            i <- 0
            for (k in freq_index){
                summation <- sum(data*exp(-1i*2*pi*k*(1:len)/len))
                spec[i] <- (1/len)*abs(summation)^2
                i <- i+1
            }
            
            freq <- freq_index * 2 * pi/len
            
            # Return as list
            return(list(freq=freq,density=spec))
        })
