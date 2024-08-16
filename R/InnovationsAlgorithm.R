innovations_step <- function(ts_obj, thetas_prev = matrix(sampleACVF(ts_obj, 1)/sampleACVF(ts_obj, 0))) {
            # Validity check
            stopifnot("Input is not a time series object"=is(ts_obj,"TimeSeries"))
            validObject(ts_obj)

            len <- ts_obj@n
            cov <- sampleACVF(ts_obj,0)
            v <- cov
            out_len <- nrow(thetas_prev) + 1



            # Checking for square matrix
            stopifnot("The matrix has to be a square matrix"=nrow(thetas_prev)==ncol(thetas_prev))

            # Checking for lower triangular matrix
            if (nrow(thetas_prev) > 1){
                for (i in 1:(nrow(thetas_prev)-1)) {
                    for (j in (i+1):ncol(thetas_prev)) {
                        if(thetas_prev[i,j]!=0) {
                            stop("The matrix has to be a lower triangular matrix")
                        }
                    }
                }
            }

            # Checking for unallowed entries
            stopifnot("Entry in the matrix is not numeric"=is.numeric(thetas_prev))
            stopifnot("NA entry contained in matrix"=all(!is.na(thetas_prev)))

            # Compute v's with known thetas
            if (nrow(thetas_prev)!=0){
                for (n in 1:nrow(thetas_prev)) {
                    calc_sum <- 0
                    for (j in 1:n){
                        calc_sum <- v[j] * thetas_prev[n, n - j + 1]^2
                    }
                    v <- c(v, cov - calc_sum)
                }
            }

            # Initialize theta matrix
            theta <- matrix(0,out_len,out_len)
            theta[1:nrow(thetas_prev), 1:ncol(thetas_prev)] <- thetas_prev

            acf_compl <- c(sapply(1:(ts_obj@n-1), \(x) {sampleACVF(ts_obj, x)}), 0)

            # Computing the new theta coefficients
            for (k in 0:(out_len-1)) {
                if (k == 0){
                    theta[out_len, out_len] <- acf_compl[out_len] / v[1]
                } else {
                    sum_thetas <- 0
                    for (j in 0:(k-1)) {
                        sum_thetas <- sum_thetas + theta[k,k-j]*theta[out_len,out_len-j]*v[j+1]
                    }
                    # Filling in the values
                    theta[out_len,out_len-k] <- 1/v[k+1] * (acf_compl[out_len-k] - sum_thetas)
                }
            }
            # Returning updated matrix, which has one additional row and column
            return(theta)
        }


#'Predictor based on the \code{Innovations} algorithm
#'
#'@description Takes a \code{TimeSeries} object and predicts a specified amount of steps. Computations are based on the \code{Innovations} algorithm.
#'
#'@details This algorithm utilizes the sample autocovariance function \code{ACF} as estimator for the autocovariance.
#'
#'@param ts_obj A stationary time series, must be a \code{TimeSeries} class.
#'@param pred_len Number of steps one wants to predict, must be length-one numeric.
#'@param entire_ts Logical value, which determins whether the original time series with predictions appended (TRUE) or only the predictions (FALSE) is returned.
#'
#'@return The return value is a \code{TimeSeries} object. Depending on the choice of \code{entire_ts}, we either obtain both the original time series with appended predictions or only the predictions made by the algorithm.
#'
#'@examples ma_ts <- MA(ma_params = 0.5, n = 50, sd = 1)
#'innovations_predictor(ma_ts, pred_len=5, entire_ts = FALSE)
#'
#'@export

innovations_predictor <-  function (ts_obj, pred_len = 1, entire_ts = TRUE){

            # Validity Check
            stopifnot("Input is not a time series object"=is(ts_obj,"TimeSeries"))
            validObject(ts_obj)
            # Checking whether pred_len and entire_ts are specified properly
            stopifnot("entire_ts not logical"=is.logical(entire_ts))
            stopifnot("pred_len not numeric"=is.numeric(pred_len))
            stopifnot("pred_len must be greater or equal to 1"=pred_len>=1)
            stopifnot("pred_len not applicable"=pred_len%%1==0)




            # Initial computation of theta matrix, needed for computing X_hats
            thetas <- innovations_step(ts_obj)
            for (n in 3:(ts_obj@n-1)){
                thetas <- innovations_step(ts_obj, thetas_prev = thetas)
            }

            X_hat <- 0
            len <- ts_obj@n

            # Compute X_hat_1 until X_hat_n
            for (n in 1:(len-1)){
                X <- ts_obj@data[1:n]
                theta_calc <- thetas[n, ][1:n]
                new_X <- sum(theta_calc * rev(X - X_hat))
                X_hat <- c(X_hat, new_X)
            }

            # Compute predictions
            for (h in 1:pred_len){
                # Update theta matrix one step
                thetas <- innovations_step(ts_obj, thetas_prev = thetas)
                theta_calc <- thetas[len - 1 + h ,]
                pre <- 0
                for (j in h:(len - 1 + h)){
                    pre <-  pre + theta_calc[j] * (ts_obj@data[len+h-j] - X_hat[len+h-j])
                }
                # Append predicted value to time series (needed for further estimates)
                ts_obj@data <- c(ts_obj@data, pre)
                ts_obj@n <- ts_obj@n + 1
            }

            # Produce output according to entire_ts
            if(entire_ts){
                vec <- ts_obj@data
                return(as(vec,"TimeSeries"))
            } else {
                vec <- ts_obj@data[(len+1):ts_obj@n]
                return(as(vec,"TimeSeries"))
            }
}
