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
    freq_index <- seq(-floor((len-1)/2),floor(len/2))
    
    #Berechnung Periodogramm
    i <- 0
    for (k in freq_index){
        summation <- sum(data*exp(-1i*2*pi*k*(1:len)/len))
        spec[i] <- (1/len)*abs(summation)^2
        i <- i+1
    }
    #Rückgabe als Liste
    
    freq <- freq_index * 2 * pi/len
    return(list(freq=freq,density=spec))
}

periodogram(ar_time_series)


