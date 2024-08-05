periodogram_gen <- function(TS_obj){
    data <- TS_obj@data #i.e., the contained time series 
    T = length(data)
    f <- function(k){
        if (!is.integer(k) | k > T/2 | k < 0) 
            stop("k must be integer between 0 and T/2 (including endpoints)")
        w <- k/T
        cos_trans <- 0
        sin_trans <- 0
        for (t in 1:T){
            cos_trans <- cos_trans + data[t]*cos(2*pi*w*t)
            sine_trans <-sin_trans + data[t]*sin(2*pi*w*t) 
        }
        return((1/T)*(cos_trans^2 + sine_trans^2))
    }
    return(f)
}

#here, input k is k-th harmonic frequency (right??), corresponding to spectral density
#evaluated at k/T, where T is number of data points.
