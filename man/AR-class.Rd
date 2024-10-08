% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Classes.R
\docType{class}
\name{AR-class}
\alias{AR-class}
\title{A S4 class to represent an AR time series}
\description{
Child class of \code{TimeSeries}, used for AR(p) time series. \code{p} refers to the length of \code{ar_params}.
}
\section{Slots}{

\describe{
\item{\code{ar_params}}{Numeric vector containing the parameters for generating an AR time series, length must be equal to the length of \code{start_values}.}

\item{\code{start_values}}{Numeric vector containing the start values of an AR time series, length must be equal to the length of \code{ar_params}.}
}}

