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
    stopifnot("Input is not a time series object"=is(ts_obj,"TimeSeries"))
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


#'Plotting the SACVF or SACF
#'
#'@description The function plots the SACVF/SACF of a given time series.
#'
#'@param ts_obj A time series, must be a \code{TimeSeries} class.
#'@param acf logical. Plots the SACVF if FALSE, SACF otherwise .
#'@param max_lag maximum lag at which to calculate the SACVF/SACF .
#'
#'@return The return value is a plot of the SACF/SACVF of a time series.
#'
#'@examples
#'ar_time_series <- AR(start_values = c(1, 1), ar_params = c(0.6, 0.6), sd = 1, n = 100)
#'plot_SACVF(ar_time_series,acf=FALSE,max_lag=20)
#'
#'@export

plot_SACVF <- function(ts_obj,acf=FALSE,max_lag=NULL) {

  # Validity check
  stopifnot("Input is not a time series object"=is(ts_obj,"TimeSeries"))
  validObject(ts_obj)

  if(is.null(max_lag)){
    max_lag <- ts_obj@n-1
  }

  # Extract data
  timeseries <- ts_obj@data
  sacvf <- sapply(0:max_lag,function(x){
    return(sampleACVF(ts_obj,x))
  })

  # Plot of sacvf
  if(acf){
    sacf <- sacvf / sacvf[1 ]
    tibble2plot <- tibble::tibble(SACF = sacf, Lag = (0:max_lag))
    plt_base <- ggplot2::ggplot(data = tibble2plot, mapping = ggplot2::aes(x = Lag, y = SACF))
    labs <- ggplot2::ggtitle("SACF")
  }
  else{
    tibble2plot <- tibble::tibble(SACVF = sacvf, Lag = (0:max_lag))
    plt_base <- ggplot2::ggplot(data = tibble2plot, mapping = ggplot2::aes(x = Lag, y = SACVF))
    labs <- ggplot2::ggtitle("SACVF")
    }

  lay <- ggplot2::geom_line(color = "purple")
  point <- ggplot2::geom_point(color = "royalblue")

  plt <- plt_base + lay + point + labs + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 15))
  plt
}

#' spectral density plotting for AR/MA time serie
#'
#'@description The function plots the spectral density of a given AR/MA-time series.
#'
#'@param ts_obj A time series, must be a \code{AR} or \code{MA} class.
#'
#'@param n Number of points to be plotted.
#'
#'@return The return value is a plot of the spectral_density of a time series.
#'
#'@examples
#'ar_time_series <- AR(start_values = c(1, 1), ar_params = c(0.6, 0.6), sd = 1, n = 100)
#'plot_spectral_density(ar_time_series)
#'ar_time_series_2 <- AR(ar_params = c(1,-0.9), n = 100, start_values = c(1,0.1))
#'plot_spectral_density(ar_time_series_2)
#'
#'@export

plot_spectral_density <- function(ts_obj, n = 10^3) {

    # Validity check
    stopifnot("Input is not a time series object"=is(ts_obj,"TimeSeries"),
              "n must be a positive integer" = is.numeric(n) && n > 0 && n == as.integer(n))
    validObject(ts_obj)



    if (is(ts_obj,"AR")){
        header <- paste0("AR(",length(ts_obj@ar_params),")")
    }
    else if (is(ts_obj,"MA")){
        header <- paste0("MA(",length(ts_obj@ma_params),")")
    }
    else stop("Spectral density not available")

    # Create spectral density data
    spect_dens <- spectral_density(ts_obj)
    freq <- seq(0,0.5,by=0.5/n)
    data <- spect_dens(freq)

    # Plot of spectral density
    tibble2plot <- tibble::tibble(Spect_dens = data, Frequency = freq)
    title <- ggplot2::ggtitle(header)
    ylab <- ggplot2::ylab("Spectral Density")
    plt_base <- ggplot2::ggplot(data = tibble2plot, mapping = ggplot2::aes(x = Frequency, y = Spect_dens))
    lay <-  ggplot2::geom_line(color = "royalblue")
    point <- ggplot2::geom_point(color = "royalblue", size = 0.5)
    labs <- ggplot2::ggtitle("Spectral Density")
    plt <- plt_base + lay + point + labs + ylab+ title + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 15))
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
    stopifnot("Input is not a time series object"=is(ts_obj,"TimeSeries"))
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
    lay <-  ggplot2::geom_line(color = "purple")
    point <- ggplot2::geom_point(color = "royalblue")
    labs <- ggplot2::ggtitle("Periodogram")
    plt <- plt_base + lay + point + labs + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 15))
    plt
}
#'TimeSeries Overview
#'
#'@description The function plots a the timeseries, its SACVF, spectral density(if available) and periodogram.
#'
#'@param ts_obj A time series, must be a \code{TimeSeries} class.
#'
#'@return The return value is a composition plot consisting of three or four plots (depending on the availability of the spectral density).
#'
#'@examples
#'ar_time_series <- AR(start_values = c(1, 1), ar_params = c(0.6, 0.6), sd = 1, n = 100)
#'plot_periodogram(ar_time_series)
#'ar_time_series_2 <- AR(ar_params = c(1,-0.9), n = 100, start_values = c(1,0.1))
#'plot_periodogram(ar_time_series_2)
#'
#'@export
plot_ts_overview <- function(ts_obj){
  plt1 <- plot_timeseries(ts_obj)
  plt2 <- plot_SACVF(ts_obj)
  plt4 <- plot_periodogram(ts_obj)
  if(is(ts_obj,"AR") ||is(ts_obj,"MA")){
      plt3 <- plot_spectral_density(ts_obj)
      (plt1 + plt2) /( plt3 + plt4)
  }
  else{
    layout <- "AABB
                AABB
                #CC#
                #CC#"
    plt1 + plt2 + plt4 +
    plot_layout(design = layout)

  }

}


