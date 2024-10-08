#'Function returning the spectral density function for a given AR/MA time series
#'
#'@description Takes a \code{TimeSeries} object. If TimeSeries is of \code{AR} or \code{MA} class, its corresponding spectral density function is computed using the AR/MA parameters. Otherwise an error message is produced.
#'
#'@details The returned spectral density function is defined for frequencies in the interval from 0 to 0.5. For each given frequency, the spectral density function computes the corresponding spectral density.
#'
#'@param ts_obj A time series, must be a \code{TimeSeries} class.
#'
#'@return The return value is the spectral density function for the given TimeSeries object.
#'
#'@examples
#'AR1 <- AR(ar_params = 0.5, start_values = 1, n = 50, sd = 1)
#'spect_dens <- spectral_density(AR1)
#'spect_dens(0.2)
#'@references  Peter J. Brockwell, Richard A. Davis (2016) \emph{Introduction to Time-Series Analysis and Forecasting}; Springer Verlag.
#'@export

spectral_density <- function(ts_obj){

    # Validity Check
    stopifnot("Input is not a time series object"=is(ts_obj,"TimeSeries"))
    validObject(ts_obj)

    if (is(ts_obj,"AR")){
        ar_params <- ts_obj@ar_params
        p <- length(ar_params)
        index <- 1:p
        sd <- ts_obj@sd
        returnvalue <- numeric(0)
        spect_dens <- function(ws){
            for (w in ws){
                if (w < 0 | w > 0.5)
                    stop("w must lie in interval [0,0.5]")

                exp_terms <- exp(-2*pi*1i*w*index)
                sum <- sum(ar_params*exp_terms)

                returnvalue <- c(returnvalue,1/((Mod(1-sum))^2))
            }
            returnvalue <- sd^2*returnvalue
            return(returnvalue)
        }
        return(spect_dens)
    }
    if (is(ts_obj,"MA")){
        ma_params <- ts_obj@ma_params
        q <- length(ma_params)
        index <- 1:q
        sd <- ts_obj@sd
        returnvalue <- numeric(0)
        spect_dens <- function(ws){
            for (w in ws){
                if (w < 0 | w > 0.5)
                    stop("w must lie in interval [0,0.5]")

                exp_terms <- exp(-2*pi*1i*w*index)
                sum <- sum(ma_params*exp_terms)

                returnvalue <- c(returnvalue,(Mod(1+sum))^2)
            }
            returnvalue <- sd^2*returnvalue
            return(returnvalue)
        }
        return(spect_dens)
    }
    else
        stop("Spectral Density not available")
}
