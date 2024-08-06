periodogram_gen <- function(ts_obj){
    data <- ts_obj@data
    len <- ts_obj@n
    f <- function(k) {
        if (!is.integer(k) | k > len/2 | k < 0) 
            stop("k must be integer between 0 and len/2 (including endpoints)")
        w <- k/len
        cos_trans <- 0
        sin_trans <- 0
        for (t in 1:len){
            cos_trans <- cos_trans + data[t]*cos(2*pi*w*t)
            sin_trans <- sin_trans + data[t]*sin(2*pi*w*t) 
        }
        return((1/len)*(cos_trans^2 + sin_trans^2))
    }
    return(f)
}

#here, input k is k-th harmonic frequency (right??), corresponding to spectral density
#evaluated at k/len, where len is number of data points.
