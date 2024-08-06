innovations_algorithm <- function(ts_obj) {
    len <- ts_obj@n
    cov <- ACF(ts_obj,0)
    v <- rep(0,len)
    v[1] <- cov
    theta <- matrix(0,len,len)
    acf_compl <- c( sapply(1:(len-1), \(x) {ACF(ts_obj, x)}), 0)
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


innovations_algorithm(ar_time_series)

# First implementation of a predictor for the innovations algorithm.
# entire_ts TRUE returns both predictions and entered TS data, whereas FALSE will only return the predicted values.
# Note: Value returned will always be a ts_obj
innovations_predict <- function (ts_obj, steps = 1, entire_ts = TRUE){
    thetas <- innovations_algorithm(ts_obj)
    X_hat <- 0
    len <- ts_obj@n
    for (n in 1:len){
        X <- ts_obj@data[1:n]
        theta_calc <- thetas[n, ][1:n]
        new_X <- sum(theta_calc * rev(X - X_hat))
        X_hat <- c(X_hat, new_X)
    }
    return(X_hat[len+1])
}


innovations_predict(ar_time_series)
plot(ar_time_series@data)
