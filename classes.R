
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
    #contains="VIRTUAL",
    prototype = list(sd=1,n=0,data=NA_real_)
    
)

setClass(
    "AR",
    slots = list(
        ar_params = "numeric",
        start_values= "numeric"
    ),
    contains="TimeSeries",
    prototype = list(ar_params = NA_real_,start_values=NA_real_)
    
)

setClass(
    "MA",
    slots = list(
        ma_params = "numeric"
    ),
    contains="TimeSeries",
    prototype = list(ma_params = NA_real_)
    
)

# Benutzerdefinierte Konstruktoren
AR <- function(ar_params = NA_real_,start_values=NA_real_,n=1,sd=1) {
    p <- length(ar_params)
    
    if (p>n) {
        stop("too many start values for requested length of the time series")
    }
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
    noise <- rnorm(n, 0, sd)
    
    # Berechne ersten q Werte der Zeitreihe
    # muss gecheckt werden, dass n auch wirklich größer ist als das aktuelle t in jeder Iteration
    for (t in 1:q) {
        if (t<=n) {
            if (t==1) {
                time_series[1] <- noise[1]
            } else {
                time_series[t] <- sum(ma_params*rev(noise[1:(t-1)])) + noise[t]
            }
        }
    }
    
    # Generiere weitere Werte der MA(q)-Zeitreihe (falls n>q)
    if (n>q) {
        for (t in (q + 1):n) {
            time_series[t] <- sum(ma_params * rev(noise[(t-q):(t-1)])) + noise[t]
        }
    }
    
    new("MA",
        ma_params=ma_params,
        sd=sd,
        data=time_series,
        n=n)
}


#Validierungen
setValidity("TimeSeries", function(object){
    if (!is.atomic(object@n)){
        "length is not atomic"
    }
    else if (!is.numeric(object@n)){
        "length is not a numeric value"
    }
    else if (0>object@n){
        "length is a negative number"
    }
    else if (!object@n %% 1 == 0){
        "length is not a integer"
    }
    else if(is.na(object@n)){
        "length is not available"
    }
    else if (0>object@sd){
        "Standard deviation is negative"
    }
    else {
        TRUE
    }
    #TODO laenge check und sd check
}) 

setValidity("AR", function(object) {
    if (length(object@ar_params) != length(object@start_values)) {
        "The number of starting values does not coincide with the amount of coefficients"
    } 
    else if(any(is.na(object@start_values))){
        "there are NA start values"
    }
    else if(any(is.na(object@ar_params))){
        "there are NA AR-Parameters"
    }
    #else if(length(object@data)!=object@n){
    #    "length of data is not equal to noted length"
    #}
    else if(!is.numeric(object@data)){
        "Data is not numeric"
    }
    else if (any(is.na(object@data))){
        print(object@data)
        "there are NA values in the data"
    }
    else {
        TRUE
    }
})

setValidity("MA", function(object) {
    if(any(is.na(object@ma_params))){
        "there are NA MA-Parameters"
    } else if(!is.numeric(object@ma_params)){
        "MA-Parameters not numeric"
    }
    #else if(length(object@data)!=object@n){
    #    "length of data is not equal to noted length"
    #}
    else if(!is.numeric(object@data)){
        "Data is not numeric"
    }
    else if (any(is.na(object@data))){
        "there are NA values in the data"
    }
    else {
    TRUE
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


set.seed(187)
m <- 175
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

#----------------------------
# auto covariance funktion (hier wird angenommen, dass ts_obj ein Klassenobjekt ist)
ACF <- function(ts_obj,h){
    validObject(ts_obj)
    ts <- ts_obj@data
    n <- ts_obj@n
    stopifnot("Input is not of type numeric"=class(ts)=="numeric")  # Do we still need this check? I assume this is done in the Validate Section
    stopifnot("index out of bounds"=abs(h)<ts_obj@n)
    
    smpl_mean <- mean(ts)
    
    summe <-  (1/n) * (ts[(1+abs(h)):n]-smpl_mean) %*% (ts[1:(n-abs(h))]-smpl_mean)
    return(summe[1][1])
}

vec_to_ts <- function(vec) {
    stopifnot("NA values in the vector"=any(is.na(vec))==FALSE,
              "length of the vector is 0"=length(vec)!=0)
    len <- length(vec)
    sd_vec <- sd(vec)
    new("TimeSeries",
        sd = sd_vec,
        n = len,
        data = vec)
}

plot(sapply(1:(ma_time_series@n-1),function(h){ACF(ma_time_series,h)}))


