periodogram <- function(ts_obj){
    #Validation
    validObject(ts_obj)
    
    data <- ts_obj@data
    len <- ts_obj@n
    #Zeitreihe ausreichend lang
    stopifnot("Die Zeitreihe muss mehr als ein Element enthalten."=len>1)

    #Periodogrammspeicher
    spec <- numeric(len)
    #Frequenzbereich
    freq_index <- seq(-floor((ts_obj@n-1)/2),floor(ts_obj@n/2))
    
    #Berechnung Periodogramm
    i <- 0
    for (k in freq_index){
        summation <- sum(ts_obj@data*exp(-1i*2*pi*k*(1:ts_obj@n)/ts_obj@n))
        spec[i] <- (1/ts_obj@n)*abs(summation)^2
        i <- i+1
    }
    #Rückgabe als Liste
    return(periodogram=spec)
}

periodogram(ar_time_series)


