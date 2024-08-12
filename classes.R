
# Laden der erforderlichen Bibliothek
library(methods)

#S4-Klassendefinitionen
setClass(
    "TimeSeries",
    slots = list(
        sd = "numeric",
        n = "numeric",
        data = "numeric"
    ),
    prototype = list(sd=1,n=0,data=numeric(0))
    
)

setClass(
    "AR",
    slots = list(
        ar_params = "numeric",
        start_values= "numeric"
    ),
    contains="TimeSeries",
    prototype = list(ar_params = numeric(0),start_values=numeric(0))
    
)

setClass(
    "MA",
    slots = list(
        ma_params = "numeric"
    ),
    contains="TimeSeries",
    prototype = list(ma_params = numeric(0))
    
)

# Benutzerdefinierte Konstruktoren
AR <- function(ar_params = numeric(0),start_values=numeric(0),n=1,sd=1) {
    p <- length(ar_params)

    stopifnot("too many start values for requested length of the time series"=p<=n)

    # Initialisieren der Zeitreihe mit Nullen
    time_series <- numeric(n)
    
    # White-Noise-Komponente generieren
    noise <- rnorm(n-p , 0, sd)
    
    # Initialisieren der ersten p Werte
    time_series[1:p] <- start_values
    
    # Generieren weiterer Werte der AR(p)-Zeitreihe (falls n>p)
    if (n>p) {
        for (t in (p + 1):n) {
            time_series[t] <- sum(ar_params * rev(time_series[(t-p):(t-1)])) + noise[t-p]
        }
    }
    
    # Entfernen der zusätzlichen Initialisierungswerte
    #time_series <- time_series[(p + 1):(n + p)]
    
    
    new("AR",
        ar_params=ar_params,
        start_values=start_values,
        n=n,
        sd=sd,
        data= time_series)
}

MA <- function(ma_params = NA_real_,sd=1,n=1) {
    q <- length(ma_params)
    
    # Initialisieren der Zeitreihe mit Nullen
    time_series <- numeric(n)
    
    # White-Noise-Komponente generieren
    noise <- rnorm(n+q, 0, sd)
    
    # Berechne die Werte der Zeitreihe
    for (t in 1:n) {
        time_series[t] <- sum(ma_params * rev(noise[t:(t+q-1)])) + noise[t+q]
    }
    
    new("MA",
        ma_params=ma_params,
        sd=sd,
        data=time_series,
        n=n)
}


#Validierungen
setValidity("TimeSeries", function(object){
    errors <- character(0)
    
    #checking n
    if(is.na(object@n)|| length(object@n)== 0){
        errors <- c(errors,"length is not available")
    }
    else{
        if (!is.atomic(object@n)){
        errors <- c(errors,"length is not atomic")
        }
        else if (!is.numeric(object@n)){
            errors <- c(errors,"length is not a numeric value")
        }
        else{
            if (0>object@n){
                errors <- c(errors,"length is not a positive number")
            }
            if (!object@n %% 1 == 0){
                errors <- c(errors,"length is not a integer")
            }
        }
    }
    #checking standard deviation
    if(!is.na(object@sd) && length(object@sd)!= 0){
    
        if(!is.atomic(object@sd)){
            errors <- c(errors,'standard deviation is not atomic')
        }
        else if (!is.numeric(object@sd)){
            errors <- c(errors,"standard deviation is not a number")
        }
        
        else if (0>object@sd){
            errors <- c(errors,"standard deviation is negative")
        }
       
    }
    
    #checking data
    if( length(object@data)== 0){
        errors <- c(errors,"data is not available")
    }
    else if (!is.numeric(object@data)){
        errors <- c(errors,"data is not a numeric vector")
    }
    else{
        if(any(is.na(object@data))){
            errors <- c(errors,"data has missing values")
        }
        if(object@n!=length(object@data)){
            errors <- c(errors,"data length doesnt correspond to the saved data length")
        }
    }
    
    
    
    
    if (length(errors)==0){
        return(TRUE)
    }
    else{
        return(errors)
    }
  
}) 

setValidity("AR", function(object) {
    errors <- character(0)
    
    if (length(object@ar_params) != length(object@start_values)) {
        errors <- c(errors,"The number of starting values does not coincide with the amount of coefficients")
    } 
    
    #Checking AR-Parameters
    if(length(object@ar_params)!=0){
        if(any(is.na(object@ar_params))){
            errors <- c(errors,"there are missing AR parameters")
        }
        if(!is.numeric(object@ar_params)){
            errors <- c(errors,"the AR parameters are not numeric")
        }
        
    }
    
    #Checking start values
    if(length(object@start_values)!=0){
        if(!is.numeric(object@start_values)){
            errors <- c(errors,"the start values are not numeric")
        }
        else{
            if( any(is.na(object@start_values))){
                errors <- c(errors,"there are missing start values")
            }
            if( any(object@data[1:length(object@start_values)]!=object@start_values)){
                errors <- c(errors,"the start of the time series differs from the start values")
            }
        }
    }
    
    if (length(errors)==0){
        return(TRUE)
    }
    else{
        return(errors)
    }
})

setValidity("MA", function(object) {
    errors <- character(0)
    
    
    #Checking MA-Parameters
    if(length(object@ma_params)!=0){
        if(any(is.na(object@ma_params))){
            errors <- c(errors,"there are missing MA parameters")
        }
        if(!is.numeric(object@ma_params)){
            errors <- c(errors,"the MA parameters are not numeric")
        }
        
    }
    
    if (length(errors)==0){
        return(TRUE)
    }
    else{
        return(errors)
    }
})

# Resampling
setGeneric(
    name = "resample",
    def = function(object) {
        standardGeneric("resample")
    }
)

setMethod(
    "resample",
    "AR",
    function(object) {
        AR(ar_params = object@ar_params, 
           start_values = object@start_values, 
           n = object@n, 
           sd = object@sd)
    }
)


# Methode zur Generierung von MA(q) Daten
setMethod(
    "resample",
    "MA",
    function(object) {
        MA(ma_params = object@ma_params,
           n = object@n,
           sd = object@sd)
    }
)


# We do not need checks here, because the validity functions will be called after the constructor
setGeneric("vec_to_ts", 
           function(object) standardGeneric("vec_to_ts"))
setMethod("vec_to_ts",
          "numeric", function(vec) {
    len <- length(vec)
    sd_vec <- sd(vec) #empirical standard deviation
    new("TimeSeries",
        sd = sd_vec,
        n = len,
        data = vec)
})

set.seed(187)
m <- 17
sd <- 1
# Beispielnutzung AR
ar_params <- c(0.3, 0.7)  # AR(2)-Modell

start_values = c(1,1)
# Erstellen eines Objekts der ARTimeSeries-Klasse
ar_time_series <- AR(start_values = start_values, ar_params = ar_params, sd = sd,n=m)


# Plotten der AR(p)-Zeitreihe
plot(ar_time_series@data)


# Beispielnutzung MA
ma_params <- c(0.3, 0.7)  # MA(2)-Modell
# Erstellen eines Objekts der MATimeSeries-Klasse
ma_time_series <- MA(ma_params = ma_params, sd = sd,n=m)


# Plotten der MA(q)-Zeitreihe
plot(ma_time_series@data)



#####Periodogram

setGeneric("periodogram", 
           function(ts_obj) standardGeneric("periodogram"))

setMethod("periodogram",
          "TimeSeries",
          function(ts_obj){
              data <- ts_obj@data
              n <-  ts_obj@n
              f <- function(ws){
                  returnvalue <- numeric(0)
                  for (w in ws){
                      if (abs(n * w - round(n * w)) > .Machine$double.eps^0.5 || w > 1/2 || w < 0){
                          stop("w must be on form w = j/n, 0 <= j <= n/2, where n is size of dataset")}
                      cos_trans <- 0
                      sin_trans <- 0
                      for (t in 1:n){
                          cos_trans <- cos_trans + data[t]*cos(2*pi*w*t)
                          sin_trans <-sin_trans + data[t]*sin(2*pi*w*t) 
                      }
                      returnvalue <-  c(returnvalue,((1/n)*(cos_trans^2 + sin_trans^2)))
                  }
                  return(returnvalue)
              }
              
              return(f)
          })



setMethod("plot_periodogram",
          "TimeSeries",
          function(ts_obj){
              n <- ts_obj@n
              p <- periodogram(ts_obj)
              xs <- (0:floor(n/2))/n
              ys <- p(xs) 
              plot(xs, ys, type = "h", xlab = "Frequency", ylab = "Periodogram")
          })

#Example/test
AR2 <- AR(c(1,-0.9),c(1,0.1),n=10000, sd = 1)
plot_periodogram(AR2)




