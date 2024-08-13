library(methods)
library(ggplot2)
library(tibble)


plot_timeseries <- function(ts_obj, prd = NULL, title = "Timeseries (with prediction)") {
    # Check if ts_obj is a valid timeseries
    validObject(ts_obj)
    
    # Extract data
    timeseries <- ts_obj@data
    
    if (is.null(prd)) {
        # Create plots
        tb <- tibble::tibble(Value = timeseries, Time = seq_along(timeseries))
        plt1 <- ggplot2::ggplot(data = tb, mapping = ggplot2::aes(x = Time, y = Value))
        line <- ggplot2::geom_line(color = "purple")
        jitter <- ggplot2::geom_jitter(color = "royalblue",shape=4)
        layout <- ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 17))
        title <- ggplot2::ggtitle(title)
        plt <- plt1 + line + jitter + title + layout
        plt
    } else {
        # Check if prd is valid timeseries
        validObject(prd)
        # Create plots with forecasts
        tb <- tibble::tibble(Value = timeseries, Time = seq_along(timeseries))
        
        prd_tbl <- tibble::tibble(Value = prd@data, Time = length(timeseries) + seq_along(prd@data))
        
        tb_combine <- rbind(tb, prd_tbl)
    
        plt2 <- ggplot2::ggplot(data = tb_combine, mapping = ggplot2::aes(x = Time, y = Value))
        lay1 <- ggplot2::geom_line(color = "purple")
        point <- ggplot2::geom_point(data = tb_combine[1:ts_obj@n,], shape=4, colour="royalblue")
        point2 <- ggplot2::geom_point(data = tb_combine[(ts_obj@n+1):(ts_obj@n+prd@n),],shape=8,colour="green")
        name <- ggplot2::labs(shape = "")
        title <- ggplot2::ggtitle(title)
        theme <- ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 17))
        plt <- plt2 + lay1 + point + point2 + title + name+ theme
        plt
    }
    
}

# Example AR-timeseries
n <- 100
sd <- 1
ar_params <- c(0.6, 0.6)
start_values <- c(1, 1)

# Create AR-timeseries
ar_time_series <- AR(start_values = start_values, ar_params = ar_params, sd = sd, n = n)


# Plot AR-timeseries
plot_timeseries(ar_time_series, title = "AR Time Series Plot")


# Plot timeseries with prediction
prd <- rep(1, 10) 
prv <- vec_to_ts(prd)
plot_timeseries(ar_time_series, title = "AR Time Series with Forecast",prd=prv)



# Plot of periodogram
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
    labs <- ggplot2::ggtitle("Timeseries Periodogram")
    plt <- plt_base + lay + point + labs + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 15))
    plt
}

# Example
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
