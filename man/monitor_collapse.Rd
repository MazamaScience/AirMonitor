% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monitor_collapse.R
\name{monitor_collapse}
\alias{monitor_collapse}
\title{Collapse an \code{mts_monitor} object into a single time series}
\usage{
monitor_collapse(
  monitor,
  longitude = NULL,
  latitude = NULL,
  deviceID = "generatedID",
  FUN = mean,
  na.rm = TRUE,
  ...
)
}
\arguments{
\item{monitor}{\emph{mts_monitor} object.}

\item{longitude}{Longitude of the collapsed time series.}

\item{latitude}{Latitude of the collapsed time series.}

\item{deviceID}{Device identifier for the collapsed time series.}

\item{FUN}{Function used to collapse multiple time series.}

\item{na.rm}{Logical specifying whether NA values should be ignored when FUN
is applied.}

\item{...}{additional arguments to be passed on to the \code{apply()} function.}
}
\value{
A \emph{mts_monitor} object representing a single time series. (A list with
\code{meta} and \code{data} dataframes.)
}
\description{
Collapses data from all time series in a \code{mts_monitor} into a
single-time series \emph{mts_monitor} object using the function provided in the
\code{FUN} argument. The single-time series result will be located at the mean
longitude and latitude unless \code{longitude} and \code{latitude}
parameters are specified.

Any columns of \code{monitor$meta} that are constant across all records will
be retained in the returned \emph{mts_monitor} \code{meta} dataframe.

The core metadata associated with this location (\emph{e.g.}
\code{countryCode, stateCode, timezone, ...}) will be determined from
the most common (or average) value found in \code{monitor$meta}. This will be
a reasonable assumption for the vast majority of intended use cases where
data from multiple instruments in close proximity are averaged together.
}
\note{
After \code{FUN} is applied, values of \code{+/-Inf} and \code{NaN} are
converted to \code{NA}. This is a convenience for the common case where
\code{FUN = min/max} or \code{FUN = mean} and some of the time steps have all
missing values. See the R documentation for \code{min} for an explanation.
}
\examples{
library(AirMonitor)

# Lane County, Oregon AQSIDs all begin with "41039"
LaneCounty <-
  NW_Megafires \%>\%
  monitor_filter(stringr::str_detect(AQSID, '^41039')) \%>\%
  monitor_filterDate(20150821, 20150828)

# Get min/max for all monitors
LaneCounty_min <- monitor_collapse(LaneCounty, deviceID = 'LaneCounty_min', FUN = min)
LaneCounty_max <- monitor_collapse(LaneCounty, deviceID = 'LaneCounty_max', FUN = max)

# Create plot
monitor_timeseriesPlot(
  LaneCounty,
  shadedNight = TRUE,
  main = "Lane County Range of PM2.5 Values"
)

# Add min/max lines
monitor_timeseriesPlot(LaneCounty_max, col = 'red', type = 's', add = TRUE)
monitor_timeseriesPlot(LaneCounty_min, col = 'blue', type = 's', add = TRUE)
}
