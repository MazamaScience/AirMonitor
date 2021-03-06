% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/airsis_loadAnnual.R
\name{airsis_loadAnnual}
\alias{airsis_loadAnnual}
\title{Load annual AIRSIS monitoring data}
\usage{
airsis_loadAnnual(
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
A \emph{mts_monitor} object with AIRSIS data. (A list with
\code{meta} and \code{data} dataframes.)
}
\description{
Loads pre-generated .rda files containing annual
AIRSIS data.

If \code{archiveDataDir} is defined, data will be loaded from this local
archive. Otherwise, data will be loaded from the monitoring data repository
maintained by the USFS AirFire team.

Current year files loaded by this function are updated once per week.

For the most recent data in the last 10 days, use \code{airsis_loadLatest()}.

For daily updates covering the most recent 45 days, use \code{airsis_loadDaily()}.
}
\examples{
\dontrun{
library(AirMonitor)

# Fail gracefully if any resources are not available
try({

# See https://en.wikipedia.org/wiki/Camp_Fire_(2018)

# AIRSIS monitors during the Camp Fire
airsis_loadAnnual(2018) \\%>\\%
  monitor_filter(stateCode == "CA") \\%>\\%
  monitor_filterDate(20181101, 20181201) \\%>\\%
  monitor_dropEmpty() \\%>\\%
  monitor_leaflet()

}, silent = FALSE)
}
}
\seealso{
\code{\link{airsis_loadDaily}}

\code{\link{airsis_loadLatest}}
}
