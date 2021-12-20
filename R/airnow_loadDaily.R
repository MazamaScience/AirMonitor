#' @export
#'
#' @title Load daily AirNow monitoring data
#'
#' @param parameterName One of the EPA AQS criteria parameter names.
#' @param archiveBaseUrl Base URL for annual EPA AQS data files.
#' @param archiveBaseDir Local base directory for annual EPA AQS data files.
#' @param QC_negativeValues Type of QC to apply to negative values.
#'
#' @return A \emph{ws_monitor} object with AirNow data.
#'
#' @description Loads pre-generated .rda files containing daily
#' AirNow data.
#'
#' If \code{archiveDataDir} is defined, data will be loaded from this local
#' archive. Otherwise, data will be loaded from the monitoring data repository
#' maintained by the USFS AirFire team.
#'
#' The files loaded by this function are updated once per day and
#' contain data for the previous 45 days.
#'
#' For the most recent data in the last 10 days, use \code{airnow_loadLatest()}.
#'
#' For data extended more than 45 days into the past, use \code{airnow_loadAnnual()}.
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
#' \item{PM2.5_nowcast}
# #' \item{PRECIP}
# #' \item{RHUM}
# #' \item{SO2}
# #' \item{SRAD}
# #' \item{TEMP}
# #' \item{UV-AETH}
# #' \item{WD}
# #' \item{WS}
#' }


airnow_loadDaily <- function(
  parameterName = "PM2.5",
  archiveBaseUrl = NULL,
  archiveBaseDir = NULL,
  QC_negativeValues = c("zero", "na", "ignore")
) {

  # ----- Validate parameters --------------------------------------------------

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
    "PM2.5",
    "PM2.5_nowcast"
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
    dataUrl <- file.path(archiveBaseUrl, "daily/data")
  }

  if ( is.null(archiveBaseDir) ) {
    dataDir <- NULL
  } else {
    dataDir <- file.path(archiveBaseDir, "daily/data")
  }

  metaFileName <- sprintf("airnow_%s_daily_meta.rda", parameterName)
  dataFileName <- sprintf("airnow_%s_daily_data.rda", parameterName)

  meta <- MazamaCoreUtils::loadDataFile(metaFileName, dataUrl, dataDir)
  data <- MazamaCoreUtils::loadDataFile(dataFileName, dataUrl, dataDir)

  # Guarantee that 'meta' and 'data' match
  ids <- names(data)[-1]

  meta <-
    meta %>%
    dplyr::filter(.data$deviceDeploymentID %in% ids)

  data <-
    data %>%
    dplyr::select(dplyr::all_of(c("datetime", meta$deviceDeploymentID)))

  # Create monitor objecet
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

# ===== DEBUG ==================================================================

if ( FALSE ) {


  parameterName <- "PM2.5"
  archiveBaseUrl <- "https://data-monitoring1.airfire.org/monitoring-v2"
  archiveBaseDir <- NULL
  QC_negativeValues = "zero"




}