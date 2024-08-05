innovations_algorithm <- function(ts_obj) {
    len <- ts_obj@n
    cov <- ACF(ts_obj,0)
    v <- rep(0,len)
    v[1] <- cov
    theta <- matrix(0,len,len)
    for(n in 1:len) {
        if (n==1) {
            theta[1][1] <- 1/v[1] * cov
            v[2] <- cov - theta[1][1]^2 * v[1]
        } else {
            for (k in 0:(n-1)) {
                sum_thetas <- 0
                for (j in 0:(k-1)) {
                    sum_thetas <- sum_thetas + theta[k,k-j]*theta[n,n-j]*v[j+1]
                }
                covariance <- ACF(ts_obj,n-k)
                theta[n,n-k] <- 1/v[k+1] * (covariance - sum_thetas)
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
