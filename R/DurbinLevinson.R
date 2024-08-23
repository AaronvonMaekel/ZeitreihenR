
DLA <- function(ts_obj){

            # Validity Check
            stopifnot("Input is not a time series object"=is(ts_obj,"TimeSeries"))
            validObject(ts_obj)

            # Save number of values in time series
            max_n <- ts_obj@n

            # Computing the ACVF values
            acvf_compl <- c(sapply(1:(max_n-1), \(x) {sampleACVF(ts_obj, x)}), 0)
            # Compute coefficients
            for (i in 1:max_n) {
               if (i == 1) {
                   # Initialization
                   phi <- sampleACVF(ts_obj, 1) / sampleACVF(ts_obj, 0)
                   phi_n <- phi
                   nu <- sampleACVF(ts_obj, 0) * (1 - phi_n^2)
              } else {
                    # Recursive computation
                    val <- phi * acvf_compl[(i-1):1]

                    # Update values of phi and nu
                    phi_n <- (1/nu) * (acvf_compl[i] - sum(val))
                    phi <- phi - phi_n * rev(phi)
                    phi <- c(phi, phi_n)
                    nu <- nu * (1 - phi_n^2)
                }
            }
            return(phi)
}

#'Predictor based on the \code{Durbin-Levinson} algorithm
#'
#'@description Takes a \code{TimeSeries} object and predicts a specified amount of steps. Computations are based on the \code{Durbin-Levinson} algorithm.
#'
#'@details This algorithm utilizes the sample autocovariance function \code{sampleACF} as estimator for the autocovariance.
#'
#'@param ts_obj A stationary time series, must be a \code{TimeSeries} class.
#'@param pred_len Number of steps one wants to predict.
#'@param entire_ts Logical vector, determines whether the original time series with predictions appended (TRUE) or only the predictions (FALSE) is returned.
#'
#'@return The return value is a \code{TimeSeries} object. Depending on the choice of \code{entire_ts}, we either obtain both the original time series with appended predictions or only the predictions made by the algorithm.
#'
#'@examples
#'ma_ts <- MA(ma_params = 0.5, n = 50, sd = 1)
#'DL_predictor(ma_ts, pred_len=5, entire_ts = FALSE)
#'
#'@references  Peter J. Brockwell, Richard A. Davis (2016) \emph{Introduction to Time-Series Analysis and Forecasting}; Springer Verlag.
#'@export

DL_predictor <- function(ts_obj, pred_len=1, entire_ts = TRUE){

            # Validity Check
            stopifnot("Input is not a time series object"=is(ts_obj,"TimeSeries"))
            validObject(ts_obj)
            stopifnot("Prediction length should be greater or equal to 1" =  pred_len >= 1)
            stopifnot("Entered preditcion length not compatible" = length(pred_len) == 1)
            stopifnot("Prediction length is not an integer" = pred_len %% 1 == 0)
            stopifnot("entire_ts has to be logical" = is.logical(entire_ts))
            stopifnot("Entered value for entire_ts is not compatible" = length(entire_ts) == 1)

            # Turning object into a mean-zero time series (needed for prediction to work)
            in_mean <- mean(ts_obj@data)
            ts_obj@data <- ts_obj@data - in_mean

            in_len <- ts_obj@n
            for (j in 1:pred_len){
                # Computing estimate using coefficients obtained from the DL algorithm
                new_val <- sum(DLA(ts_obj) * rev(ts_obj@data))

                # Appending estimated value to the time series (needed for further estimates)
                ts_obj@data <- c(ts_obj@data, new_val)
                ts_obj@n <- ts_obj@n + 1
            }
            vec <- ts_obj@data + in_mean

            # Producing output according to the specification made by entire_ts
            if (entire_ts==FALSE){
                vec <- vec[(in_len + 1):(in_len + pred_len)]
            }
            return(as(vec,"TimeSeries"))
}

