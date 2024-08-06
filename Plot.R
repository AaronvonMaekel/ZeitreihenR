library(methods)
library(ggplot2)
library(tibble)

# Anpassung der Plot-Funktion für S4-Zeitreihen-Objekte
plot_timeseries <- function(ts_obj, prd = NULL, title = "Zeitreihe mit Vorhersage") {
    # Überprüfen, ob das Eingabeobjekt eine valide Zeitreihe ist
    validObject(ts_obj)
    
    # Extrahiere die Daten aus dem Zeitreihenobjekt
    timeseries <- ts_obj@data
    
    # Eingabeüberprüfung
    stopifnot("Übergabegabevektor nicht numerisch." = is.numeric(timeseries))
    stopifnot("Übergabevektor nicht lang genug." = length(timeseries) > 0)
    stopifnot("Übergabevektor nicht numerisch oder NULL." = (is.numeric(prd) | is.null(prd)))
    
    if (is.null(prd)) {
        # Plots erstellen
        tibble2plot <- tibble::tibble(Wert = timeseries, Zeit = seq_along(timeseries))
        plt_base <- ggplot2::ggplot(data = tibble2plot, mapping = ggplot2::aes(x = Zeit, y = Wert))
        lay <- ggplot2::geom_line(color = "blue")
        point <- ggplot2::geom_point(color = "red")
        title <- ggplot2::ggtitle(title)
        theme <- ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 15))
        plt <- plt_base + lay + point + title + theme
        plt
    } else {
        # Timeseries mit Vorhersage plotten
        value_tbl <- tibble::tibble(Wert = timeseries, Zeit = seq_along(timeseries))
        value_tbl$Group <- "Series"
        
        prd_tbl <- tibble::tibble(Wert = prd, Zeit = length(timeseries) + seq_along(prd))
        prd_tbl$Group <- "Forecast"
        
        tibble2plot <- rbind(value_tbl, prd_tbl)
        
        plt_base <- ggplot2::ggplot(data = tibble2plot, mapping = ggplot2::aes(x = Zeit, y = Wert))
        lay1 <- ggplot2::geom_line(color = "#F8766D")
        lay2 <- ggplot2::geom_line(ggplot2::aes(colour = factor(Group)))
        point <- ggplot2::geom_point(data = tibble2plot, ggplot2::aes(x = Zeit, y = Wert))
        labs <- ggplot2::labs(colour = "")
        title <- ggplot2::ggtitle(title)
        theme <- ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 15))
        plt <- plt_base + lay1 + lay2 + point + title + labs + theme
        plt
    }
}

# Beispielnutzung mit AR-Zeitreihe
n <- 100
sd <- 1
ar_params <- c(0.6, 0.6)
start_values <- c(1, 1)

# AR-Zeitreihe erzeugen
ar_time_series <- AR(start_values = start_values, ar_params = ar_params, sd = sd, n = n)

# Plotten der AR-Zeitreihe
plot_timeseries(ar_time_series, title = "AR Time Series Plot")

# Beispiel mit Vorhersage
prd <- rep(1, 10)  # Beispielhafte Vorhersage
plot_timeseries(ar_time_series, title = "AR Time Series with Forecast")


#Plotten des Periodogramms 
plot_periodogram <- function(ts_obj, logscale = FALSE) {
    # Überprüfen, ob das Eingabeobjekt eine valide Zeitreihe ist
    validObject(ts_obj)
    
    #Extrahiere die Daten aus dem Zeitreihenobjekt
    timeseries <- ts_obj@data
    
    # Berechne das Periodogramm
    periodogram <- periodogram_gen(ts_obj)
    
    # Eingabeüberprüfung
    stopifnot("Der Eingabevektor periodogram ist nicht numerisch." = is.numeric(periodogram))
    stopifnot("Der Vektor periodogram muss wenigstens die Länge 1 haben." = length(periodogram) > 0)
    
    # Frequenzen generieren
    n <- length(periodogram)
    k <- floor((-(n-1)/2):(n/2))
    freq <- 2 * pi * k / n
    
    # Periodogramm plotten
    tibble2plot <- tibble::tibble(Wert = periodogram, Fourierfrequenz = freq)
    plt_base <- ggplot2::ggplot(data = tibble2plot, mapping = ggplot2::aes(x = Fourierfrequenz, y = Wert))
    lay <- ggplot2::geom_line(color = "#6a93b0")
    point <- ggplot2::geom_point(color = "black")
    labs <- ggplot2::ggtitle("Periodogramm der Zeitreihe")
    
    if (logscale) {
        trans <- ggplot2::scale_y_continuous(trans = "log10")
    } else {
        trans <- NULL
    }
    
    plt <- plt_base + lay + point + labs + trans + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 15))
    plt
}

# Beispielnutzung mit einer AR-Zeitreihe
set.seed(123)
n <- 100
sd <- 1
ar_params <- c(0.6, 0.6)
start_values <- c(1, 1)

# AR-Zeitreihe erzeugen
ar_time_series <- AR(start_values = start_values, ar_params = ar_params, sd = sd, n = n)

# Periodogramm plotten
plot_periodogram(ar_time_series, logscale = FALSE)

# Periodogramm im Log-Scale plotten
plot_periodogram(ar_time_series, logscale = TRUE)
