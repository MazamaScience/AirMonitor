% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addAQIStackedBar.R
\name{addAQIStackedBar}
\alias{addAQIStackedBar}
\title{Create stacked AQI bar}
\usage{
addAQIStackedBar(
  pollutant = c("PM2.5", "CO", "OZONE", "PM10", "AQI"),
  palette = c("EPA", "subdued", "deuteranopia"),
  width = 0.01,
  height = 1,
  pos = c("left", "right")
)
}
\arguments{
\item{pollutant}{EPA AQS criteria pollutant.}

\item{palette}{Named color palette to use for AQI categories.}

\item{width}{Width of the bar as a fraction of the width of the plot area.}

\item{height}{Height of the bar as a fraction of the height of the plot area.}

\item{pos}{Position of the stacked bar relative to the plot.}
}
\value{
No return value, called to add color bars to a time series plot.
}
\description{
Draws a stacked bar indicating AQI levels on one side of a plot
The \link{monitor_timeseriesPlot} function uses this function internally when
specifying \code{addAQI = TRUE}.
}
