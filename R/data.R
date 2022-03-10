# ----- Example datasets ------------------------------------------------------

#' @encoding UTF-8
#' @title Example EPA AQS 88101 dataset
#' @format A \emph{mts_monitor} object with 16584 rows and 44 columns of data.
#' @description The \code{example_88101} dataset provides a quickly loadable
#' version of a \emph{mts_monitor} object for practicing and code examples.
#'
#' @details
#' In the summer of 2015 Washington state had several catastrophic wildfires that led
#' to many days of heavy smoke in eastern Washington, Oregon and northern Idaho.
#' The example_88101 dataset contains EPA AQS ambient monitoring data for the
#' Pacific Northwest from May 31 through November 01, 2015. All data are
#' associated with Parameter Code 88101 -- PM2.5 FRM/FEM Mass.
#'
#' This dataset was generated on 2021-10-19 by running:
#'
#' \preformatted{
#' library(AirMonitor)
#'
#' example_88101 <-
#'   epa_aqs_loadAnnual(
#'     year = 2015,
#'     parameterCode = 88101,
#'     archiveBaseUrl = NULL,
#'     archiveBaseDir = "~/Data/monitoring"
#'   ) \%>\%
#'   monitor_filterMeta(stateCode \%in\% c("WA", "OR", "ID")) \%>\%
#'   monitor_filterDate(20150601, 20151101)
#'
#' save(example_88101, file = "data/example_88101.rda")
#' }
#'
"example_88101"

#' @encoding UTF-8
#' @title Carmel Valley example dataset
#' @format A \emph{mts_monitor} object with 600 rows and 2 columns of data.
#' @description The \code{Carmel_Valley} dataset provides a quickly loadable
#' version of a \emph{mts_monitor} object for practicing and code examples.
#'
#' @details
#' In August of 2016, the Soberanes fire in California burned along the Big Sur
#' coast. It was at the time the most expensive wildifre in US history. This
#' dataset contains PM2.5 monitoring data for the monitor in Carmel Valley which
#' shows heavy smoke as well as strong diurnal cycles associated with sea
#' breezes. Data are stored as a \emph{mts_monitor} object and are used in some
#' examples in the package documentation.
#'
#' This dataset was generated on 2021-10-19 by running:
#'
#' \preformatted{
#' library(AirMonitor)
#'
#' Carmel_Valley <-
#'   epa_aqs_loadAnnual(
#'     year = 2016,
#'     parameterCode = 88101,
#'     archiveBaseUrl = NULL,
#'     archiveBaseDir = "~/Data/monitoring"
#'   ) \%>\%
#'   monitor_filterMeta(deviceDeploymentID == "a9572a904a4ed46d_060530002_03") \%>\%
#'   monitor_filterDate(20160722, 20160815)
#'
#' save(Carmel_Valley, file = "data/Carmel_Valley.rda")
#' }
#'
"Carmel_Valley"

#' @encoding UTF-8
#' @title Camp Fire example dataset
#' @format A \emph{ws_monitor} object with "meta" and "data" dataframes.
#' @description The \code{Camp_Fire} dataset provides a quickly loadable
#' version of a \emph{ws_monitor} object for practicing and code examples.
#'
#' This dataset was was generated on 2022-02-15 by running:
#'
#' \preformatted{
#' library(AirMonitor)
#'
#' Camp_Fire <-
#'   monitor_loadAnnual(2018) \%>\%
#'   monitor_filter(stateCode == 'CA') \%>\%
#'   monitor_filterDate(
#'     startdate = 20181108,
#'     enddate = 20181123,
#'     timezone = "America/Los_Angeles"
#'  ) \%>\%
#'  monitor_dropEmpty()
#'
#' save(Camp_Fire, file = "data/Camp_Fire.rda")
#' }
"Camp_Fire"
