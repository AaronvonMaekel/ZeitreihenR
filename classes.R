
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
    
    # Initialisieren der Zeitreihe mit Nullen
    time_series <- numeric(n + p)
    
    # White-Noise-Komponente generieren
    noise <- rnorm(n , 0, sd)
    
    # Initialisieren der ersten p Werte
    time_series[1:p] <- start_values
    
    # Generieren der AR(p)-Zeitreihe
    for (t in (p + 1):(n + p)) {
        time_series[t] <- sum(ar_params * rev(time_series[(t-p):(t-1)])) + noise[t-p]
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
    
    ma_params <- ma_params
    q <- length(ma_params)
    
    # Initialisieren der Zeitreihe mit Nullen
    time_series <- numeric(n)
    
    # White-Noise-Komponente generieren
    noise <- rnorm(n+q , 0, sd)
    
    
    # Generieren der MA(q)-Zeitreihe
    for (t in (q + 1):(n + q)) {
        time_series[t] <- sum(ma_params * rev(time_series[(t-q):(t-1)])) + noise[t]
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
    
    
    else{
        
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
    else if(length(object@data)!=object@n){
        "length of data is not equal to noted length"
    }
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
    else if(length(object@data)!=object@n){
        "length of data is not equal to noted length"
    }
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
        
        ar_params <- object@ar_params
        p <- length(ar_params)
        
        # Initialisieren der Zeitreihe mit Nullen
        time_series <- numeric(n + p)
        
        # White-Noise-Komponente generieren
        noise <- rnorm(n , 0, object@sd)
        
        # Initialisieren der ersten p Werte
        time_series[1:p] <- object@start_values
        
        # Generieren der AR(p)-Zeitreihe
        for (t in (p + 1):(n + p)) {
            time_series[t] <- sum(ar_params * rev(time_series[(t-p):(t-1)])) + noise[t]
        }
        
        # Entfernen der zusätzlichen Initialisierungswerte
        time_series <- time_series[(p + 1):(n + p)]
        object@data = time_series
        
    }
)


# Methode zur Generierung von MA(q) Daten
setMethod(
    "resample",
    "MA",
    function(object) {
        
        ma_params <- object@ma_params
        q <- length(ma_params)
        
        # Initialisieren der Zeitreihe mit Nullen
        time_series <- numeric(n)
        
        # White-Noise-Komponente generieren
        noise <- rnorm(n+q , 0, object@sd)
        
        
        # Generieren der MA(q)-Zeitreihe
        for (t in (q + 1):(n + q)) {
            time_series[t] <- sum(ma_params * rev(time_series[(t-q):(t-1)])) + noise[t]
        }
        
        return(time_series)
    }
)




set.seed(123)
n <- 10
sd <- 1
# Beispielnutzung AR
ar_params <- c(0.6, 0.6)  # AR(2)-Modell

start_values = c(1,1)
# Erstellen eines Objekts der ARTimeSeries-Klasse
ar_time_series <- AR(start_values = start_values, ar_params = ar_params, sd = sd,n=10)

# Plotten der AR(p)-Zeitreihe
plot(ar_time_series@data)


# Beispielnutzung MA
ma_params <- c(0.6, 0.6)  # MA(2)-Modell
# Erstellen eines Objekts der MATimeSeries-Klasse
ma_time_series <- MA(ma_params = ma_params, sd = sd,n=n)

# Plotten der MA(q)-Zeitreihe
plot(ma_time_series@data)

#----------------------------
# auto covariance funktion (hier wird angenommen ts_obj ist eine Klassenobjekt)
ACF <- function(ts_obj,h){
    validObject(ts_obj)
    ts <- ts_obj@data
    n <- ts_obj@n
    stopifnot("Input is not of type numeric"=class(ts)=="numeric")
    stopifnot("index out of bounds"=abs(h)<ts_obj@n)
    
    smpl_mean <- mean(ts)
    
    summe <-  (1/n) * (ts[(1+abs(h)):n]-smpl_mean) %*% (ts[1:(n-abs(h))]-smpl_mean)
    return(summe)
}



plot(sapply(1:(ma_time_series@n-1),function(h){ACF(ma_time_series,h)}))
