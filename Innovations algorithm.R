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

innovations_algorithm_2 <- function(ts_obj, thetas_prev=matrix(nrow=0, ncol=0), out_len=nrow(thetas_prev)+1) {
    len <- ts_obj@n
    cov <- ACF(ts_obj,0)
    v <- cov
    
    # check matrix
    stopifnot("The matrix has to be a square matrix"=nrow(thetas_prev)==ncol(thetas_prev))
    for (i in 1:(nrow(thetas_prev)-1)) {
        for (j in (i+1):ncol(thetas_prev)) {
            if(thetas_prev[i,j]!=0) {
                stop("The matrix has to be a lower triangular matrix")
            }
        }
    }
    
    # check out len groesser als die matrix
    stopifnot("out_len has to be greater than the number of rows in the matrix"=out_len>nrow(thetas_prev))
    
    # We should also check for "unallowed" values in the matrix, such as NA or letters!!!
    
    for (n in 1:nrow(thetas_prev)) {
        v <- c(v, cov - sum(rev(thetas_prev[n,])^2 * v))
    }
    
    theta <- matrix(0,out_len,out_len)
    theta[1:nrow(thetas_prev), 1:ncol(thetas_prev)] <- thetas_prev
    
    acf_compl <- c(sapply(1:(len-1), \(x) {ACF(ts_obj, x)}), 0)
    for(n in (nrow(thetas_prev)+1):out_len) {
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
            v <- c(v, cov - sum_v)
        }
    }
    return(theta)
}

innovations_algorithm(ar_time_series)

# First implementation of a predictor for the innovations algorithm.
# entire_ts TRUE returns both predictions and entered TS data, whereas FALSE will only return the predicted values.
# Note: Value returned will always be a ts_obj
innovations_predict <- function (ts_obj, steps = 1, entire_ts = TRUE){
    thetas <- innovations_algorithm_2(ts_obj, out_len = ts_obj@n)
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
