% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monitor_load.R
\name{monitor_load}
\alias{monitor_load}
\title{Load monitoring data from all sources}
\usage{
monitor_load(
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  archiveBaseUrl = paste0("https://airfire-data-exports.s3.us-west-2.amazonaws.com/",
    "monitoring/v2"),
  archiveBaseDir = NULL,
  QC_negativeValues = c("zero", "na", "ignore"),
  epaPreference = c("airnow", "epa_aqs")
)
}
\arguments{
\item{startdate}{Desired start datetime (ISO 8601).}

\item{enddate}{Desired end datetime (ISO 8601).}

\item{timezone}{Olson timezone used to interpret dates.}

\item{archiveBaseUrl}{Base URL for monitoring v2 data files.}

\item{archiveBaseDir}{Local base directory for monitoring v2 data files.}

\item{QC_negativeValues}{Type of QC to apply to negative values.
files are available from both `epa` and `airnow`.}

\item{epaPreference}{Preferred data source for EPA data when annual data
files are available from both `epa_aqs` and `airnow`.}
}
\value{
A \emph{mts_monitor} object with PM2.5 monitoring data. (A list with
\code{meta} and \code{data} dataframes.)
}
\description{
Loads monitoring data for a given time range. Data from AirNow,
AIRSIS and WRCC are combined into a single \emph{mts_monitor} object.

Archival datasets are combined with 'daily' and 'latest' datasets as needed to
satisfy the requested date range.
}
\examples{
\dontrun{
library(AirMonitor)

# Fail gracefully if any resources are not available
try({

wa <-
  monitor_load(20210601, 20211001) \%>\%
  monitor_filter(stateCode == "WA")

monitor_timeseriesPlot(wa)

}, silent = FALSE)
}
}
\seealso{
\code{\link{monitor_loadAnnual}}

\code{\link{monitor_loadDaily}}

\code{\link{monitor_loadLatest}}
}
