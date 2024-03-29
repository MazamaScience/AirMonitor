% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/airnow_loadMonthly.R
\name{airnow_loadMonthly}
\alias{airnow_loadMonthly}
\title{Load monthly AirNow monitoring data}
\usage{
airnow_loadMonthly(
  monthStamp = NULL,
  archiveBaseUrl = paste0("https://airfire-data-exports.s3.us-west-2.amazonaws.com/",
    "monitoring/v2"),
  archiveBaseDir = NULL,
  QC_negativeValues = c("zero", "na", "ignore"),
  parameterName = "PM2.5"
)
}
\arguments{
\item{monthStamp}{Year-month [YYYYmm].}

\item{archiveBaseUrl}{Base URL for monitoring v2 data files.}

\item{archiveBaseDir}{Local base directory for monitoring v2 data files.}

\item{QC_negativeValues}{Type of QC to apply to negative values.}

\item{parameterName}{One of the EPA AQS criteria parameter names.}
}
\value{
A \emph{mts_monitor} object with AirNow data. (A list with
\code{meta} and \code{data} dataframes.)
}
\description{
Loads pre-generated .rda files containing hourly AirNow data.

If \code{archiveDataDir} is defined, data will be loaded from this local
archive. Otherwise, data will be loaded from the monitoring data repository
maintained by the USFS AirFire team.

The files loaded by this function contain a single month's worth of data

For the most recent data in the last 10 days, use \code{airnow_loadLatest()}.

For daily updates covering the most recent 45 days, use \code{airnow_loadDaily()}.

For data extended more than 45 days into the past, use \code{airnow_loadAnnual()}.

Pre-processed AirNow exists for the following parameters:

\enumerate{
\item{PM2.5}
}
}
