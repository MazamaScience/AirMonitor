#' @export
#' @importFrom dplyr across
#'
#' @title Load annual AIRSIS monitoring data
#'
#' @param year Year [YYYY].
#' @param archiveBaseUrl Base URL for monitoring v2 data files.
#' @param archiveBaseDir Local base directory for monitoring v2 data files.
#' @param QC_negativeValues Type of QC to apply to negative values.
#'
#' @return A \emph{mts_monitor} object with AIRSIS data. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @description Loads pre-generated .rda files containing annual
#' AIRSIS data.
#'
#' If \code{archiveDataDir} is defined, data will be loaded from this local
#' archive. Otherwise, data will be loaded from the monitoring data repository
#' maintained by the USFS AirFire team.
#'
#' Current year files loaded by this function are updated once per week.
#'
#' For the most recent data in the last 10 days, use \code{airsis_loadLatest()}.
#'
#' For daily updates covering the most recent 45 days, use \code{airsis_loadDaily()}.
#'
#' @seealso \code{\link{airsis_loadDaily}}
#' @seealso \code{\link{airsis_loadLatest}}
#'
#' @examples
#' \dontrun{
#' library(AirMonitor)
#'
#' # Fail gracefully if any resources are not available
#' try({
#'
#' # See https://en.wikipedia.org/wiki/Camp_Fire_(2018)
#'
#' # AIRSIS monitors during the Camp Fire
#' airsis_loadAnnual(2018) \%>\%
#'   monitor_filter(stateCode == "CA") \%>\%
#'   monitor_filterDate(20181101, 20181201) \%>\%
#'   monitor_dropEmpty() \%>\%
#'   monitor_leaflet()
#'
#' }, silent = FALSE)
#' }

airsis_loadAnnual <- function(
  year = NULL,
  archiveBaseUrl = paste0(
    "https://airfire-data-exports.s3.us-west-2.amazonaws.com/",
    "monitoring/v2"
  ),
  archiveBaseDir = NULL,
  QC_negativeValues = c("zero", "na", "ignore")
) {

  parameterName <- "PM2.5"

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(year)
  MazamaCoreUtils::stopIfNull(parameterName)

  if ( as.numeric(year) < 2014 )
    stop(paste0("No ARISIS data is available before 2014"))

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
    dataUrl <- file.path(archiveBaseUrl, "airsis", year, "data")
  }

  if ( is.null(archiveBaseDir) ) {
    dataDir <- NULL
  } else {
    dataDir <- file.path(archiveBaseDir, "airsis", year, "data")
  }

  metaFileName <- sprintf("airsis_%s_%s_meta.rda", parameterName, year)
  dataFileName <- sprintf("airsis_%s_%s_data.rda", parameterName, year)

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


  year <- 2021
  archiveBaseUrl <- "https://airfire-data-exports.s3.us-west-2.amazonaws.com/monitoring/v2"
  archiveBaseDir <- NULL
  QC_negativeValues = "zero"




}
