#'Plot functions for a time series
#'
#'@description The function provides plots for a given time series.
#'
#'@param ts_obj A time series, must be a \code{TimeSeries} class.
#'@param prd Prediction of a time series (optional), must be a \code{TimeSeries} class.
#'
#'@return The return value is a plot of the time series.
#'
#'@examples
#'ar_time_series <- AR(start_values = c(1, 1), ar_params = c(0.6, 0.6), sd = 1, n = 100)
#'plot_timeseries(ar_time_series)
#'
#'@export

plot_timeseries <- function(ts_obj, prd = NULL) {
    # Validity check
    validObject(ts_obj)

    # Extract data
    timeseries <- ts_obj@data
    tb <- tibble::tibble(Value = timeseries, Time = seq_along(timeseries))

    # Creating title
    header <- "Time series"

    if (is(ts_obj,"AR")){
        header <- paste0("AR(",length(ts_obj@ar_params),")-",header)
    }
    else if (is(ts_obj,"MA")){
        header <- paste0("MA(",length(ts_obj@ma_params),")-",header)
    }

    # Create the plot with correct data
    if (is.null(prd)) {
        plt <- ggplot2::ggplot(data = tb, mapping = ggplot2::aes(x = Time, y = Value))
    }
    else {
        header <- paste(header,"with prediction")

        #Validity check of prediction object
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

    # Extra parameters for prediction
    if (!is.null(prd)) {
        point2 <- ggplot2::geom_point(data = prd_tbl,shape=8,colour="green")
        name <- ggplot2::labs(shape = "")
        plt <- plt  + point2 + name
    }
    plt
}

#'Plot function for the periodogram
#'
#'@description The function plots the periodogram of a given time series.
#'
#'@param ts_obj A time series, must be a \code{TimeSeries} class.
#'
#'@return The return value is a plot of the periodogram of a time series.
#'
#'@examples
#'ar_time_series <- AR(start_values = c(1, 1), ar_params = c(0.6, 0.6), sd = 1, n = 100)
#'plot_periodogram(ar_time_series)
#'ar_time_series_2 <- AR(ar_params = c(1,-0.9), n = 100, start_values = c(1,0.1))
#'plot_periodogram(ar_time_series_2)
#'
#'@export

plot_periodogram <- function(ts_obj) {

    # Validity check
    validObject(ts_obj)

    # Extract data
    timeseries <- ts_obj@data

    # Create periodogram
    periodogram <- periodogram(ts_obj)
    freq <- periodogram$freq
    data <- periodogram$density

    # Cutting off negative values
    data <- data[freq>=0]
    freq <- freq[freq>=0]

    # Plot of periodogram
    tibble2plot <- tibble::tibble(Spectrum = data, FourierFrequency = freq)
    plt_base <- ggplot2::ggplot(data = tibble2plot, mapping = ggplot2::aes(x = FourierFrequency, y = Spectrum))
    lay <- ggplot2::geom_line(color = "purple")
    point <- ggplot2::geom_point(color = "royalblue")
    labs <- ggplot2::ggtitle("Periodogram")
    plt <- plt_base + lay + point + labs + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 15))
    plt
}
