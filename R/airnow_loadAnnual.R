#' @export
#' @importFrom dplyr across
#'
#' @title Load annual AirNow monitoring data
#'
#' @param year Year [YYYY].
#' @param parameterName One of the EPA AQS criteria parameter names.
#' @param archiveBaseUrl Base URL for monitoring v2 data files.
#' @param archiveBaseDir Local base directory for monitoring v2 data files.
#' @param QC_negativeValues Type of QC to apply to negative values.
#'
#' @return A \emph{mts_monitor} object with AirNow data. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @description Loads pre-generated .rda files containing hourly AirNow data.
#'
#' If \code{archiveDataDir} is defined, data will be loaded from this local
#' archive. Otherwise, data will be loaded from the monitoring data repository
#' maintained by the USFS AirFire team.
#'
#' The files loaded by this function contain a single year's worth of data
#'
#' For the most recent data in the last 10 days, use \code{airnow_loadLatest()}.
#'
#' For daily updates covering the most recent 45 days, use \code{airnow_loadDaily()}.
#'
#' For archival data for a specific month, use \code{airnow_loadMonthly()}.
#'
#' Pre-processed AirNow exists for the following parameters:
#' \enumerate{
# #' \item{BARPR}
# #' \item{BC}
# #' \item{CO}
# #' \item{NO}
# #' \item{NO2}
# #' \item{NO2Y}
# #' \item{NO2X}s
# #' \item{NOX}
# #' \item{NOOY}
# #' \item{OC}
# #' \item{OZONE}
# #' \item{PM10}
#' \item{PM2.5}
# #' \item{PM2.5_nowcast}
# #' \item{PRECIP}
# #' \item{RHUM}
# #' \item{SO2}
# #' \item{SRAD}
# #' \item{TEMP}
# #' \item{UV-AETH}
# #' \item{WD}
# #' \item{WS}
#' }
#'
#' @seealso \code{\link{airnow_loadDaily}}
#' @seealso \code{\link{airnow_loadLatest}}
#' @seealso \code{\link{airnow_loadMonthly}}
#'
#' @examples
#' \dontrun{
#' library(AirMonitor)
#'
#' # Fail gracefully if any resources are not available
#' try({
#'
#' # See https://en.wikipedia.org/wiki/2017_Montana_wildfires
#'
#' # Daily Barplot of Montana wildfires
#' airnow_loadAnnual(2017) \%>\%
#'   monitor_filter(stateCode == "MT") \%>\%
#'   monitor_filterDate(20170701, 20170930, timezone = "America/Denver") \%>\%
#'   monitor_dailyStatistic() \%>\%
#'   monitor_timeseriesPlot(
#'     ylim = c(0, 300),
#'     xpd = NA,
#'     addAQI = TRUE,
#'     main = "Montana 2017 -- AirNow Daily Average PM2.5"
#'   )
#'
#' }, silent = FALSE)
#' }


airnow_loadAnnual <- function(
  year = NULL,
  archiveBaseUrl = paste0(
    "https://airfire-data-exports.s3.us-west-2.amazonaws.com/",
    "monitoring/v2"
  ),
  archiveBaseDir = NULL,
  QC_negativeValues = c("zero", "na", "ignore"),
  parameterName = "PM2.5"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(year)
  MazamaCoreUtils::stopIfNull(parameterName)

  QC_negativeValues <- match.arg(QC_negativeValues)

  if ( is.null(archiveBaseUrl) && is.null(archiveBaseDir) )
    stop("one of 'archiveBaseUrl' or 'archiveBaseDir' must be defined")

  # Parameter code
  validParameterNames <- c(
    # "BARPR",
    # "BC",
    # "CO",
    # "NO",
    # "NO2",
    # "NO2Y",
    # "NO2X",
    # "NOX",
    # "NOOY",
    # "OC",
    # "OZONE",
    # "PM10",
    "PM2.5"
    # "PM2.5_nowcast"
    # "PRECIP",
    # "RHUM",
    # "SO2",
    # "SRAD",
    # "TEMP",
    # "UV-AETH",
    # "WD",
    # "WS"
  )

  parameterName <- as.character(parameterName)
  if ( !parameterName %in% validParameterNames ) {
    stop(sprintf(
      "data for parameterName '%s' has not been processed",
      parameterName
    ))
  }

  # ----- Load data ------------------------------------------------------------

  # Create file name and path according to the AirMonitorIngest scheme

  if ( is.null(archiveBaseUrl) ) {
    dataUrl <- NULL
  } else {
    dataUrl <- file.path(archiveBaseUrl, "airnow", year, "data")
  }

  if ( is.null(archiveBaseDir) ) {
    dataDir <- NULL
  } else {
    dataDir <- file.path(archiveBaseDir, "airnow", year, "data")
  }

  metaFileName <- sprintf("airnow_%s_%s_meta.rda", parameterName, year)
  dataFileName <- sprintf("airnow_%s_%s_data.rda", parameterName, year)

  meta <- MazamaCoreUtils::loadDataFile(metaFileName, dataUrl, dataDir)
  data <- MazamaCoreUtils::loadDataFile(dataFileName, dataUrl, dataDir)

  # Guarantee that 'meta' and 'data' match
  ids <- names(data)[-1]

  meta <-
    meta %>%
    dplyr::filter(.data$deviceDeploymentID %in% ids)

  data <-
    data %>%
    dplyr::select(dplyr::all_of(c("datetime", meta$deviceDeploymentID))) %>%
    # Replace any NaN that snuck in
    dplyr::mutate(across(tidyselect::vars_select_helpers$where(is.numeric), function(x) ifelse(is.nan(x), NA, x)))

  # Create monitor object
  monitor <- list(meta = meta, data = data)

  monitor <- structure(monitor, class = c("mts_monitor", "mts", class(monitor)))

  MazamaTimeSeries::mts_check(monitor)

  # ----- Apply QC -------------------------------------------------------------

  if ( QC_negativeValues == "zero" ) {

    monitor <- monitor_replaceValues(monitor, data < 0, 0)

  } else if ( QC_negativeValues == "na" ) {

    monitor <- monitor_replaceValues(monitor, data < 0, as.numeric(NA))

  }

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}

