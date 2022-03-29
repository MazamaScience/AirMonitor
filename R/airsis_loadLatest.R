#' @export
#' @importFrom dplyr across
#'
#' @title Load most recent AIRSIS monitoring data
#'
#' @param archiveBaseUrl Base URL for monitoring v2 data files.
#' @param archiveBaseDir Local base directory for monitoring v2 data files.
#' @param QC_negativeValues Type of QC to apply to negative values.
#'
#' @return A \emph{mts_monitor} object with AIRSIS data. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @description Loads pre-generated .rda files containing the most recent
#' AIRSIS data.
#'
#' If \code{archiveDataDir} is defined, data will be loaded from this local
#' archive. Otherwise, data will be loaded from the monitoring data repository
#' maintained by the USFS AirFire team.
#'
#' The files loaded by this function are updated multiple times an hour and
#' contain data for the previous 10 days.
#'
#' For daily updates covering the most recent 45 days, use \code{airsis_loadDaily()}.
#'
#' For data extended more than 45 days into the past, use \code{airsis_loadAnnual()}.
#'
#' @seealso \code{\link{airsis_loadAnnual}}
#' @seealso \code{\link{airsis_loadDaily}}
#'
#' @examples
#' \dontrun{
#' library(AirMonitor)
#'
#' # Fail gracefully if any resources are not available
#' try({
#'
#' airsis_loadLatest()\ %>\%
#'   monitor_filter(stateCode == "CA") \%>\%
#'   monitor_leaflet()
#'
#' }, silent = FALSE)
#' }

airsis_loadLatest <- function(
  archiveBaseUrl = paste0(
    "https://airfire-data-exports.s3.us-west-2.amazonaws.com/",
    "monitoring/v2"
  ),
  archiveBaseDir = NULL,
  QC_negativeValues = c("zero", "na", "ignore")
) {

  parameterName <- "PM2.5"

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
    dataUrl <- file.path(archiveBaseUrl, "latest/data")
  }

  if ( is.null(archiveBaseDir) ) {
    dataDir <- NULL
  } else {
    dataDir <- file.path(archiveBaseDir, "latest/data")
  }

  metaFileName <- sprintf("airsis_%s_latest_meta.rda", parameterName)
  dataFileName <- sprintf("airsis_%s_latest_data.rda", parameterName)

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

# ===== DEBUG ==================================================================

if ( FALSE ) {


  archiveBaseUrl <- "https://airfire-data-exports.s3.us-west-2.amazonaws.com/monitoring/v2"
  archiveBaseDir <- NULL
  QC_negativeValues = "zero"




}
