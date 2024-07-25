

# Laden der erforderlichen Bibliothek
library(methods)
setClass(
    "TimeSeriesGenerator" 
    
)
# Definition der S4-Klasse für AR(p) Zeitreihe
setClass(
    "AR_Generator",
    slots = list(
        #n = "numeric",
        ar_params = "numeric",
        start_values= "numeric",
        sd = "numeric"
        #data = "numeric"
    ),
    contains="TimeSeriesGenerator",
    prototype = list(ar_params = NA_real_,start_values=NA_real_,sd=1)
    
)

# Benutzerdefinierter Konstruktor, nur benötigt wenn new nicht ausreicht
#AR_Generator <- function(ar_params = NA_real_,start_values=NA_real_,sd=1) {
#custom stuff
#    new("AR_Generator", ar_params=ar_params, start_values=start_values,sd=sd)
#}

setValidity("TimeSeriesGenerator", function(object){
    TRUE
})

setValidity("AR_Generator", function(object) {
    if (length(ar_params) != length(start_values)) {
        "The number of starting values does not coincide with the amount of coefficients"
    } else {
        TRUE
    }
})


# Methode zur Generierung von AR(p) Daten
setGeneric(
    name = "generateData",
    def = function(object,n) {
        standardGeneric("generateData")
    }
)

setMethod(
    "generateData",
    "AR_Generator",
    function(object,n) {
        
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
        
        return(time_series)
    }
)

# Beispielnutzung der S4-Klasse
set.seed(123)
n <- 10
ar_params <- c(0.6, 0.6)  # AR(2)-Modell
sd <- 1
start_values = c(1,1)
# Erstellen eines Objekts der ARTimeSeries-Klasse
ar_time_gen <- new("AR_Generator",start_values = start_values, ar_params = ar_params, sd = sd)

# Generieren der AR(p)-Zeitreihe
ar_time_series <- generateData(ar_time_gen,n)

# Plotten der AR(p)-Zeitreihe
plot(ar_time_series)

