#' @export
#'
#' @title Load annual monitoring data from all sources
#'
#' @param year Year [YYYY].
#' @param archiveBaseUrl Base URL for monitoring v2 data files.
#' @param archiveBaseDir Local base directory for monitoring v2 data files.
#' @param QC_negativeValues Type of QC to apply to negative values.
#' @param epaPreference Preferred data source for EPA data when annual data
#' files are available from both `epa_aqs` and `airnow`.
#'
#' @return A \emph{mts_monitor} object with PM2.5 monitoring data. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @description Combine annual data from AirNow, AIRSIS and WRCC:
#'
#' If \code{archiveDataDir} is defined, data will be loaded from this local
#' archive. Otherwise, data will be loaded from the monitoring data repository
#' maintained by the USFS AirFire team.
#'
#' Current year files loaded by this function are updated once per week.
#'
#' For the most recent data in the last 10 days, use \code{monitor_loadLatest()}.
#'
#' For daily updates covering the most recent 45 days, use \code{monitor_loadDaily()}.
#'
#' For data extended more than 45 days into the past, use \code{monitor_load()}.
#'
#' @note The AirNow data stream contains data that may also be available from AIRSIS
#' and WRCC. This can be detected by looking at the `locationID` associated with
#' each time series. When \code{epaPreference = "airnow"}, time series from
#' AIRSIS or WRCC that share a `locationID` found in the AirNow data are removed
#' so that each location is represented by a single time series coming from AirNow.
#'
# #' @seealso \code{\link{monitor_load}}
#' @seealso \code{\link{monitor_loadDaily}}
#' @seealso \code{\link{monitor_loadLatest}}
#' @examples
#' \dontrun{
#' library(AirMonitor)
#' # Fail gracefully if any resources are not available
#' try({
#'
#' monitor_loadAnnual() %>%
#'   monitor_filter(stateCode %in% CONUS) %>%
#'   monitor_leaflet()
#'
#' }, silent = FALSE)
#' }

monitor_loadAnnual <- function(
    year = NULL,
    archiveBaseUrl = paste0(
      "https://airfire-data-exports.s3.us-west-2.amazonaws.com/",
      "monitoring/v2"
    ),
    archiveBaseDir = NULL,
    QC_negativeValues = c("zero", "na", "ignore"),
    epaPreference = c("airnow", "epa_aqs")
) {

  parameterName <- "PM2.5"

  # ----- Validate parameters --------------------------------------------------

  # Cutoff years
  firstAirnowYear <- 2014
  firstAirsisYear <- 2004
  firstWrccYear <- 2010
  firstEpa88101Year <- 2008
  firstEpa88502Year <- 1998

  MazamaCoreUtils::stopIfNull(year)
  year <- as.numeric(year)
  if ( year < firstEpa88502Year )
    stop("no data available prior to 1998")

  QC_negativeValues <- match.arg(QC_negativeValues)

  if ( is.null(archiveBaseUrl) && is.null(archiveBaseDir) )
    stop("one of 'archiveBaseUrl' or 'archiveBaseDir' must be defined")

  epaPreference <- match.arg(epaPreference)

  # Override epaPreference if year is before any AirNow data
  if ( year < firstAirnowYear )
    epaPreference <- "epa_aqs"

  # ----- Load data ------------------------------------------------------------

  monitorList <- list()

  if ( year >= firstAirnowYear ) {

    if ( epaPreference == "airnow" ) {

      # AirNow annual files
      try({
        monitorList[["airnow"]] <- airnow_loadAnnual(year, archiveBaseUrl, archiveBaseDir, QC_negativeValues, parameterName)
      }, silent = TRUE)

    } else {

      # EPA AQS 88101 + 88502 files
      try({
        monitorList[["epa_aqs"]] <- epa_aqs_loadAnnual(year, archiveBaseUrl, archiveBaseDir, QC_negativeValues, parameterCode = "PM2.5")
      }, silent = TRUE)

    }

  } else {

    # EPA AQS 88101 + 88502 files
    if ( year >= firstEpa88101Year ) {
      try({
        monitorList[["epa_aqs"]] <- epa_aqs_loadAnnual(year, archiveBaseUrl, archiveBaseDir, QC_negativeValues, parameterCode = "PM2.5")
      }, silent = TRUE)
    }

  }

  # AIRSIS annual files
  if ( year >= firstAirsisYear ) {
    try({
      monitorList[["airsis"]] <- airsis_loadAnnual(year, archiveBaseUrl, archiveBaseDir, QC_negativeValues)
    }, silent = TRUE)
  }

  # WRCC annual files
  if ( year >= firstWrccYear ) {
    try({
      monitorList[["wrcc"]] <- wrcc_loadAnnual(year, archiveBaseUrl, archiveBaseDir, QC_negativeValues)
    }, silent = TRUE)
  }

  monitor_all <-
    monitor_combine(monitorList) %>%
    monitor_dropEmpty()

  # ----- Remove duplicate locations -------------------------------------------

  # NOTE:  This applies only to AirNow data, not EPA AQS data.
  # NOTE:
  # NOTE:  Whenever we have multiple monitors reporting from the same location,
  # NOTE:  we always favor the data fom AirNow over AIRSIS and WRCC.
  # NOTE:  Because airnow comes first in monitor_combine() above, AirNow data
  # NOTE:  will be preferentially retained.

  if ( year >= firstAirnowYear && epaPreference == "airnow" ) {

    ids <-
      monitor_all$meta %>%
      dplyr::distinct(.data$locationID, .keep_all = TRUE) %>%
      dplyr::pull(.data$deviceDeploymentID)

    monitor <- monitor_select(monitor_all, ids)

  } else {

    monitor <- monitor_all

  }

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}
