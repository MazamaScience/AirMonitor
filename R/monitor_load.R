#' @export
#'
#' @title Load monitoring data from all sources
#'
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param timezone Olson timezone used to interpret dates.
#' @param archiveBaseUrl Base URL for monitoring v2 data files.
#' @param archiveBaseDir Local base directory for monitoring v2 data files.
#' @param QC_negativeValues Type of QC to apply to negative values.
#' files are available from both `epa` and `airnow`.
#' @param epaPreference Preferred data source for EPA data when annual data
#' files are available from both `epa_aqs` and `airnow`.
#'
#' @description Loads monitoring data for a given time range. Data from AirNow,
#' AIRSIS and WRCC are combined into a single \emph{mts_monitor} object.
#'
#' Archival datasets are combined with 'daily' and 'latest' datasets as needed to
#' satisfy the requested date range.
#'
#' @seealso \code{\link{monitor_loadAnnual}}
#' @seealso \code{\link{monitor_loadDaily}}
#' @seealso \code{\link{monitor_loadLatest}}
#'
#' @return A \emph{mts_monitor} object with PM2.5 monitoring data. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @examples
#' \dontrun{
#' library(AirMonitor)
#'
#' # Fail gracefully if any resources are not available
#' try({
#'
#' wa <-
#'   monitor_load(20210601, 20211001) %>%
#'   monitor_filter(stateCode == "WA")
#'
#' monitor_timeseriesPlot(wa)
#'
#' }, silent = FALSE)
#' }

monitor_load <- function(
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  archiveBaseUrl = paste0(
    "https://airfire-data-exports.s3.us-west-2.amazonaws.com/",
    "monitoring/v2"
  ),
  archiveBaseDir = NULL,
  QC_negativeValues = c("zero", "na", "ignore"),
  epaPreference = c("airnow", "epa_aqs")
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(startdate)
  MazamaCoreUtils::stopIfNull(enddate)
  QC_negativeValues <- match.arg(QC_negativeValues)
  epaPreference <- match.arg(epaPreference)

  # Deal with missing timezones
  if ( is.null(timezone) ) {
    if ( lubridate::is.POSIXct(startdate) ) {
      timezone <- lubridate::tz(startdate)
    } else {
      timezone <- "UTC"
    }
  }

  tRange <- MazamaCoreUtils::dateRange(
    startdate = startdate,
    enddate = enddate,
    timezone = timezone,
    unit = "hour",
    ceilingStart = FALSE,
    ceilingEnd = FALSE
  )

  starttime <- tRange[1]
  endtime <- tRange[2]

  now <- lubridate::now(tzone = "UTC")
  now_m1 <- now - lubridate::ddays(1)
  now_m10 <- now - lubridate::ddays(10)
  now_m45 <- now - lubridate::ddays(45)

  # ----- Load annual data -----------------------------------------------------

  if ( starttime < now_m45 ) {

    y1 <- lubridate::year(starttime)
    y2 <- lubridate::year(endtime)

    monitorList <- list()

    # NOTE:  Failure to load any annual data for a given year is not trapped.
    for ( year in y1:y2 ) {
      monitorList[[as.character(year)]] <- monitor_loadAnnual(year, archiveBaseUrl, archiveBaseDir, QC_negativeValues, epaPreference)
    }

    annualData <- monitor_combine(monitorList)

  }

  # ----- Load daily data ------------------------------------------------------

  if ( endtime >= now_m45 && starttime < now_m10 ) {

    dailyData <- monitor_loadDaily(archiveBaseUrl, archiveBaseDir, QC_negativeValues)

  }

  # ----- Load latest data -----------------------------------------------------

  if ( starttime >= now_m10 || endtime >= now_m1 ) {

    latestData <- monitor_loadLatest(archiveBaseUrl, archiveBaseDir, QC_negativeValues)

  }

  # ----- Join mts_monitor objects ---------------------------------------------

  if ( exists("annualData") ) {
    monitor <- annualData
  }

  if ( exists("dailyData") ) {
    if ( exists("mts_monitor") ) {
      monitor <- monitor_combine(monitor, dailyData)
    } else {
      monitor <- dailyData
    }
  }

  if ( exists("latestData") ) {
    if ( exists("mts_monitor") ) {
      monitor <- monitor_combine(monitor, latestData)
    } else {
      monitor <- latestData
    }
  }

  # Filter to the requested time range
  monitor <- monitor_filterDate(monitor, starttime, endtime)

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}
