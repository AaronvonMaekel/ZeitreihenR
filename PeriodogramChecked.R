periodogram <- function(ts_obj){
    #hier erst Validierung ob ts so wohldefiniert?
    data <- ts_obj@data
    len <- ts_obj@n
    #Zeitreihe ausreichend lang
    if (len <= 1){
        stop("Die Zeitreihe muss mehr als ein Element enthalten.")
    }
    #Periodogrammspeicher
    spec <- numeric(len)
    #Frequenzbereich
    freq_index <- seq(-floor((n-1)/2),floor(n/2))
    
    #Berechnung Periodogramm
    for (k in freq_index){
        summation <- sum(data*exp(-1i*2*pi*k*(1:n)/n))
        spec[k+ceiling(n/2)] <- (1/n)*abs(summation)^2
    }
    #Rückgabe als Liste
    return(periodogram=spec)
}

periodogram(ar_time_series)
