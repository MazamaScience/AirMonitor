% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monitor_loadAnnual.R
\name{monitor_loadAnnual}
\alias{monitor_loadAnnual}
\title{Load annual monitoring data from all sources}
\usage{
monitor_loadAnnual(
  year = NULL,
  archiveBaseUrl = paste0("https://airfire-data-exports.s3.us-west-2.amazonaws.com/",
    "monitoring/v2"),
  archiveBaseDir = NULL,
  QC_negativeValues = c("zero", "na", "ignore")
)
}
\arguments{
\item{year}{Year [YYYY].}

\item{archiveBaseUrl}{Base URL for monitoring v2 data files.}

\item{archiveBaseDir}{Local base directory for monitoring v2 data files.}

\item{QC_negativeValues}{Type of QC to apply to negative values.}
}
\value{
A \emph{mts_monitor} object with PM2.5 monitoring data. (A list with
\code{meta} and \code{data} dataframes.)
}
\description{
Combine annual data from AirNow, AIRSIS and WRCC:

If \code{archiveDataDir} is defined, data will be loaded from this local
archive. Otherwise, data will be loaded from the monitoring data repository
maintained by the USFS AirFire team.

Current year files loaded by this function are updated once per week.

For the most recent data in the last 10 days, use \code{monitor_loadLatest()}.

For daily updates covering the most recent 45 days, use \code{monitor_loadDaily()}.

For data extended more than 45 days into the past, use \code{monitor_load()}.
}
\note{
The AirNow data stream contains data may also be available from AIRSIS
and WRCC. This can be detected by looking at the `locationID` associated with
each time series. Wherever multiple time series share a `locationID`, the
time series from AirNow or WRCC are removed so that each location is represented
by a single time series coming from AirNow.
}
\examples{
\dontrun{
library(AirMonitor)
# Fail gracefully if any resources are not available
try({

monitor_loadAnnual() \%>\%
  monitor_filter(stateCode \%in\% CONUS) \%>\%
  monitor_leaflet()

}, silent = FALSE)
}
}
\seealso{
\code{\link{monitor_loadDaily}}

\code{\link{monitor_loadLatest}}
}
