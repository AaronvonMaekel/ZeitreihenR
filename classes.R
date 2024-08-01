

# Laden der erforderlichen Bibliothek
library(methods)
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
# Definition der S4-Klasse für AR(p) Zeitreihe
setClass(
    "AR",
    slots = list(
        ar_params = "numeric",
        start_values= "numeric"
    ),
    contains="TimeSeries",
    prototype = list(ar_params = NA_real_,start_values=NA_real_)
    
)

# Benutzerdefinierter Konstruktor, nur benötigt wenn new nicht ausreicht
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
        time_series[t] <- sum(ar_params * rev(time_series[(t-p):(t-1)])) + noise[t]
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

setValidity("TimeSeries", function(object){
    TRUE #TODO laenge check und sd check
}) 

setValidity("AR", function(object) {
    if (length(ar_params) != length(start_values)) {
        "The number of starting values does not coincide with the amount of coefficients"
    } else {
        TRUE
    }
})


# Methode zur Generierung von AR(p) Daten
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
        #time_series <- time_series[(p + 1):(n + p)]
        object@data = time_series
        
    }
)

# Beispielnutzung der S4-Klasse
set.seed(123)
n <- 10
ar_params <- c(0.6, 0.6)  # AR(2)-Modell
sd <- 1
start_values = c(1,1)
# Erstellen eines Objekts der ARTimeSeries-Klasse
ar_time_series <- AR(start_values = start_values, ar_params = ar_params, sd = sd,n=10)

# Plotten der AR(p)-Zeitreihe
plot(ar_time_series@data)



# Definition der S4-Klasse für MA(q) Zeitreihe
setClass(
    "MA",
    slots = list(
        ma_params = "numeric"
        ),
    contains="TimeSeries",
    prototype = list(ma_params = NA_real_)
    
)

# Benutzerdefinierter Konstruktor
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
        data=time_series)
}

setValidity("MA", function(object) {
   # if (length(ar_params) != length(start_values)) {
        "The number of starting values does not coincide with the amount of coefficients"
    #} else {
        TRUE
  #  }
})


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

# Beispielnutzung der S4-Klasse
set.seed(123)
n <- 10
ma_params <- c(0.6, 0.6)  # AR(2)-Modell
sd <- 1
# Erstellen eines Objekts der MATimeSeries-Klasse
ma_time_series <- MA(ma_params = ma_params, sd = sd,n=n)

# Plotten der MA(q)-Zeitreihe
plot(ma_time_series@data)

# auto covariance funktion (hier wird angenommen ts ist noch keine Klasse)
ACF <- function(ts,h){
    stopifnot("Input is not of type numeric"=class(ts)=="numeric")
    stopifnot("index out of bounds"=abs(h)<length(ts))
    n <- length(ts)
    smpl_mean <- mean(ts)
    
    summe <-  (1/n) * (ts[(1+abs(h)):n]-smpl_mean) %*% (ts[1:(n-abs(h))]-smpl_mean)
    return(summe)
}



ts <- ma_time_series
plot(sapply(1:(length(ts)-1),function(h){ACF(ts,h)}))
