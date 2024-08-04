dla <- function(ts_obj){
    max_n <- ts_obj@n
    acf_compl <- c( sapply(1:(max_n-1), \(x) {ACF(ts_obj, x)}), 0)
    for (n in 1:max_n) {
        if (n == 1) { 
            phi <- ACF(ts_obj, 1) / ACF(ts_obj, 0)
            phi_n <- phi
            nu <- ACF(ts_obj, 0) * (1 - phi^2)
        } else {
            # dla_step(ts_obj, j, phi, ) NOTE: Here we could (alternatively) always call a "step" function
            # However, I think that's quite difficult, as we need to return both Nu and Phi
            val <- sapply(1:(n-1), \(j) {ACF(ts_obj, n-j) * phi[j]})
            #print(val)
            phi_n <- (1/nu)*(acf_compl[n] - sum(val))
            phi <- phi - phi_n * rev(phi)
            phi <- c(phi, phi_n)
            nu <- nu*(1 - phi_n*phi_n)
        }
    }
    return(phi) 
}


dla(ar_time_series)


# First testing
ss <- dla(ar_time_series)
ne <- sum(ss * rev(ar_time_series@data))
ar_time_series@n <- ar_time_series@n + 1
ar_time_series@data <- c(ar_time_series@data, ne)
plot(ar_time_series@data)

ar_time_series@n
ar_time_series@data

#dl_predictor <- function(ts_obj, )