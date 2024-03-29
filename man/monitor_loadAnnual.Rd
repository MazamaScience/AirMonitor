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
  QC_negativeValues = c("zero", "na", "ignore"),
  epaPreference = c("airnow", "epa_aqs")
)
}
\arguments{
\item{year}{Year [YYYY].}

\item{archiveBaseUrl}{Base URL for monitoring v2 data files.}

\item{archiveBaseDir}{Local base directory for monitoring v2 data files.}

\item{QC_negativeValues}{Type of QC to apply to negative values.}

\item{epaPreference}{Preferred data source for EPA data when annual data
files are available from both `epa_aqs` and `airnow`.}
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
This function guarantees that only a single time series will be
associated with each \code{locationID} using the following logic:
\enumerate{
\item{AirNow data takes precedence over data from AIRSIS or WRCC}
\item{more recent data takes precedence over older data}
}
This relevant mostly for "temporary" monitors which may be replaced after they
are initially deployed. If you want access to all device deployments associated
with a specific \code{locationID}, you can use the provider specific functions:
\code{\link{airnow_loadAnnual}},
\code{\link{airsis_loadAnnual}} and
\code{\link{wrcc_loadAnnual}}
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
