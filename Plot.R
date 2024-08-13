# muss noch weg?
library(methods)
library(ggplot2)
library(tibble)

#'Plot functions for a time series
#'
#'@import methods
#'@import ggplot2
#'@import tibble
#'
#'@description The function plots a given time series.
#'
#'@param ts_obj A time series, must be a TimeSeries class.
#'@param prd Prediction of a time series (optional), must be a TimeSeries class.
#'
#'@return The return value is the plotted time series.
#'
#'@examples 
#'ar_time_series <- AR(start_values = c(1, 1), ar_params = c(0.6, 0.6), sd = 1, n = 100)
#'plot_timeseries(ar_time_series)
#'
#'@export

plot_timeseries <- function(ts_obj, prd = NULL) {
    # Check if ts_obj is a valid timeseries
    validObject(ts_obj)
    
    # Extract data
    timeseries <- ts_obj@data
    tb <- tibble::tibble(Value = timeseries, Time = seq_along(timeseries))
    
    #creating title
    header<- "Timeseries"
    
    if(is(ts_obj,"AR")){
      header <- paste0("AR(",length(ts_obj@ar_params),")-",header)  
    }
    else if(is(ts_obj,"MA")){
        header <- paste0("MA(",length(ts_obj@ma_params),")-",header)  
    }


    # Create the plot with the correct data
    if (is.null(prd)) {
        plt <- ggplot2::ggplot(data = tb, mapping = ggplot2::aes(x = Time, y = Value))
    }
    else {
        header <- paste(header,"with Prediction")
        #Validity Check of Prediction Object
        validObject(prd)
        prd_tbl <- tibble::tibble(Value = prd@data, Time = length(timeseries) + seq_along(prd@data))
        tb_combine <- rbind(tb, prd_tbl)
        plt <- ggplot2::ggplot(data = tb_combine, mapping = ggplot2::aes(x = Time, y = Value))
    } 
    
    
    line <- ggplot2::geom_line(color = "purple")
    title <- ggplot2::ggtitle(header)
    theme <- ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 17))
    point <- ggplot2::geom_point(data = tb, shape=4, colour="royalblue")
    plt <- plt + line + point + title + theme
    
    
    #add extra stuff if we use prediction
    if (!is.null(prd)) {    
        point2 <- ggplot2::geom_point(data = prd_tbl,shape=8,colour="green")
        name <- ggplot2::labs(shape = "")
        
        plt <- plt  + point2 + name 
        
    }
    plt
    
}


#'Plot function for the periodogram
#'
#'@import methods
#'@import ggplot2
#'@import tibble
#'
#'@description The function plots the periodogram of a given time series.
#'
#'@param ts_obj A time series, must be a TimeSeries class.
#'
#'@return The return value is the plotted periodogram of a time series.
#'
#'@examples 
#'ar_time_series <- AR(start_values = c(1, 1), ar_params = c(0.6, 0.6), sd = 1, n = 100)
#'plot_periodogram(ar_time_series)
#'ar_time_series_2 <- AR(ar_params = c(1,-0.9), n = 100, start_values = c(1,0.1))
#'plot_periodogram(ar_time_series_2)
#'
#'@export

plot_periodogram <- function(ts_obj) {
    # Check if ts_obj is valid time series
    validObject(ts_obj)
    
    # Extract data from timeseries
    timeseries <- ts_obj@data
    
    # Create periodogram
    periodogram <- periodogram(ts_obj)
    freq <- periodogram$freq
    data <- periodogram$density
    
    #cutting off negative values
    data <- data[freq>=0]
    freq <- freq[freq>=0]

    # Periodogramm plotten
    tibble2plot <- tibble::tibble(Spectrum = data, FourierFrequency = freq)
    plt_base <- ggplot2::ggplot(data = tibble2plot, mapping = ggplot2::aes(x = FourierFrequency, y = Spectrum))
    lay <- ggplot2::geom_line(color = "purple")
    point <- ggplot2::geom_point(color = "royalblue")
    labs <- ggplot2::ggtitle("Periodogram")
    plt <- plt_base + lay + point + labs + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 15))
    plt
}

# Example AR-timeseries
n <- 100
sd <- 1
ar_params <- c(0.6, 0.6)
start_values <- c(1, 1)

# Create AR-timeseries
ar_time_series <- AR(start_values = start_values, ar_params = ar_params, sd = sd, n = n)

# Plot AR-timeseries
plot_timeseries(ar_time_series)


# Plot timeseries with prediction
prd <- rep(1, 10) 
prv <- vec_to_ts(prd)
plot_timeseries(ar_time_series,prd=prv)


# Example with prediction
n <- 100
sd <- 1
ar_params <- c(0.6, 0.6)
start_values <- c(1, 1)

# Create TimeSeries
ar_time_series <- AR(start_values = start_values, ar_params = ar_params, sd = sd, n = n)

# Plot periodogram
plot_periodogram(ar_time_series)

example47 <- AR(ar_params=c(0.5,0.9),n=100,start_values=c(1,0.1))
example48 <- AR(ar_params=c(1,-0.9),n=100,start_values=c(1,0.1))
plot_periodogram(example47)
plot_periodogram(example48)

#test
library(patchwork) 
(plot_timeseries(example48)|plot_periodogram(example48))
