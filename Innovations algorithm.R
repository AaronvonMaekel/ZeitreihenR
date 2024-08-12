innovations_algorithm <- function(ts_obj) {
    len <- ts_obj@n
    cov <- ACF(ts_obj,0)
    v <- rep(0,len)
    v[1] <- cov
    theta <- matrix(0,len,len)
  
    acf_compl <- c(sapply(1:(len-1), \(x) {ACF(ts_obj, x)}), 0)
    for(n in 1:len) {
        if (n==1) {
            theta[1, 1] <- 1/v[1] * acf_compl[1]
            v[2] <- cov - theta[1, 1]^2 * v[1]
        } else {
            for (k in 0:(n-1)) {
                if (k == 0){
                    theta[n, n] <- acf_compl[n] / v[1]
                } else {
                    sum_thetas <- 0
                    for (j in 0:(k-1)) {
                        sum_thetas <- sum_thetas + theta[k,k-j]*theta[n,n-j]*v[j+1]
                    }
                    theta[n,n-k] <- 1/v[k+1] * (acf_compl[n-k] - sum_thetas)
                }
                
            }
            sum_v <- 0
            for(j in 0:(n-1)) {
                sum_v <- sum_v + theta[n,n-j]^2 * v[j+1]
            }
            v[n+1] <- cov - sum_v
        }
    }
    return(theta)
}

innovations_algorithm_2 <- function(ts_obj, thetas_prev = matrix(ACF(ts_obj, 1)/ACF(ts_obj, 0))) {
    len <- ts_obj@n
    cov <- ACF(ts_obj,0)
    v <- cov
    out_len <- nrow(thetas_prev)+1
    
    # check matrix
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
    
    # We should also check for "unallowed" values in the matrix, such as NA or letters!!!
    stopifnot("Entry in the matrix is not numeric"=is.numeric(thetas_prev))
    stopifnot("NA entry contained in matrix"=all(!is.na(thetas_prev)))
    
    
    # Compute v's
    if (nrow(thetas_prev)!=0){
        for (n in 1:nrow(thetas_prev)) {
            calc_sum <- 0
            for (j in 1:n){
                calc_sum <- v[j] * thetas_prev[n, n - j + 1]^2
            }
            v <- c(v, cov - calc_sum)
        }
    }
    
    theta <- matrix(0,out_len,out_len)
    theta[1:nrow(thetas_prev), 1:ncol(thetas_prev)] <- thetas_prev
    acf_compl <- c(sapply(1:(ts_obj@n-1), \(x) {ACF(ts_obj, x)}), 0)

    for (k in 0:(out_len-1)) {
        if (k == 0){
            theta[out_len, out_len] <- acf_compl[out_len] / v[1] 
        } else {
            sum_thetas <- 0
            for (j in 0:(k-1)) {
                sum_thetas <- sum_thetas + theta[k,k-j]*theta[out_len,out_len-j]*v[j+1]
            }
            theta[out_len,out_len-k] <- 1/v[k+1] * (acf_compl[out_len-k] - sum_thetas)
        }
                
    }
    return(theta)
}


# First implementation of a predictor for the innovations algorithm.
# entire_ts TRUE returns both predictions and entered TS data, whereas FALSE will only return the predicted values.
# Note: Value returned will always be a ts_obj

innovations_predict <- function (ts_obj, steps = 1, entire_ts = TRUE){
    
    # ToDo: Check whether entire_ts is logical and steps has a reasonable value
    stopifnot("entire_ts not logical"=is.logical(entire_ts))
    stopifnot("steps not numeric"=is.numeric(steps))
    stopifnot("steps must be greater or equal to 1"=steps>=1)
    stopifnot("stepsize not applicable"=steps%%1==0)
    
    validObject(ts_obj) # Checking that ts_obj is indeed a timeseries
    
    thetas <- innovations_algorithm_2(ts_obj)
    for (n in 3:(ts_obj@n-1)){
        thetas <- innovations_algorithm_2(ts_obj, thetas_prev = thetas)
    }
    X_hat <- 0
    len <- ts_obj@n
    
    # Compute X_hats
    for (n in 1:(len-1)){
        X <- ts_obj@data[1:n]
        theta_calc <- thetas[n, ][1:n]
        new_X <- sum(theta_calc * rev(X - X_hat))
        X_hat <- c(X_hat, new_X)
    } # Note: We have now obtained X_hat_1 to X_hat_n
    
    
    # Compute predictions
    for (h in 1:steps){
        thetas <- innovations_algorithm_2(ts_obj, thetas_prev = thetas)
        theta_calc <- thetas[len - 1 + h ,]
        pre <- 0
        for (j in h:(len - 1 + h)){
            pre <-  pre + theta_calc[j] * (ts_obj@data[len+h-j] - X_hat[len+h-j])
        }
        ts_obj@data <- c(ts_obj@data, pre)
        ts_obj@n <- ts_obj@n + 1
    }
    
    # Produce output according to entire_ts
    if(entire_ts){
        vec <- ts_obj@data
        return(vec_to_ts(vec))
    } else {
        vec <-  ts_obj@data[(len+1):ts_obj@n]
        return(vec_to_ts(vec))
    }
}


innovations_predict(ar_time_series, steps=7, entire_ts = FALSE)
plot(ar_time_series@data)
