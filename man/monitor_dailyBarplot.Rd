% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monitor_dailyBarplot.R
\name{monitor_dailyBarplot}
\alias{monitor_dailyBarplot}
\title{Create daily barplot}
\usage{
monitor_dailyBarplot(
  monitor = NULL,
  id = NULL,
  add = FALSE,
  addAQI = FALSE,
  palette = c("EPA", "subdued", "deuteranopia"),
  opacity = NULL,
  minHours = 18,
  dayBoundary = c("clock", "LST"),
  NAAQS = c("PM2.5_2024", "PM2.5"),
  ...
)
}
\arguments{
\item{monitor}{\emph{mts_monitor} object.}

\item{id}{\code{deviceDeploymentID} for a single time series found in \code{monitor}.
(Optional if \code{monitor} contains only a single time series.)}

\item{add}{Logical specifying whether to add to the current plot.}

\item{addAQI}{Logical specifying whether to add visual AQI decorations.}

\item{palette}{Named color palette to use when adding AQI decorations.}

\item{opacity}{Opacity to use for bars.}

\item{minHours}{Minimum number of valid hourly records per day required to
calculate statistics. Days with fewer valid records will be assigned \code{NA}.}

\item{dayBoundary}{Treatment of daylight savings time:  "clock" uses daylight
savings time as defined in the local timezone, "LST" uses "local standard time"
all year round.}

\item{NAAQS}{Version of NAAQS levels to use. See Note.}

\item{...}{Additional arguments to be passed to \code{graphics::barplot()}.}
}
\value{
No return value. This function is called to draw an air quality
daily average plot on the active graphics device.
}
\description{
Creates a daily barplot of data from a \emph{mts_monitor} object.

Reasonable defaults are chosen for annotations and plot characteristics.
Users can override any defaults by passing in parameters accepted by
\code{graphics::barplot}.
}
\note{
The underlying axis for this plot is not a time axis so you cannot use this
function to "add" bars on top of a \code{monitor_timeseriesPlot()}. See
the \pkg{AirMonitorPlots} package for more flexibility in plotting.

On February 7, 2024, EPA strengthened the National Ambient Air Quality
Standards for Particulate Matter (PM NAAQS) to protect millions of Americans
from harmful and costly health impacts, such as heart attacks and premature
death. Particle or soot pollution is one of the most dangerous forms of air
pollution, and an extensive body of science links it to a range of serious
and sometimes deadly illnesses. EPA is setting the level of the primary
(health-based) annual PM2.5 standard at 9.0 micrograms per cubic meter to
provide increased public health protection, consistent with the available
health science.
See \href{https://www.epa.gov/pm-pollution/final-reconsideration-national-ambient-air-quality-standards-particulate-matter-pm}{PM NAAQS update}.
}
\examples{
library(AirMonitor)

layout(matrix(seq(2)))

Carmel_Valley \%>\% monitor_dailyBarplot()
title("(pre-2024 PM NAAQS)", line = 0)

Carmel_Valley \%>\% monitor_dailyBarplot(NAAQS = "PM2.5_2024")
title("(updated PM NAAQS)", line = 0)

layout(1)

}
