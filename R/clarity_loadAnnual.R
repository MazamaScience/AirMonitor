#' @export
#' @importFrom dplyr across
#'
#' @title Load annual Clarity monitoring data
#'
#' @param year Year [YYYY].
#' @param archiveBaseUrl Base URL for monitoring v2 data files.
#' @param archiveBaseDir Local base directory for monitoring v2 data files.
#' @param QC_negativeValues Type of QC to apply to negative values.
#' @param QC_calibration Clairty calibration category
#'
#' @return A \emph{mts_monitor} object with Clarity data. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @description Loads pre-generated .rda files containing annual
#' Clarity data.
#'
#' If \code{archiveDataDir} is defined, data will be loaded from this local
#' archive. Otherwise, data will be loaded from the monitoring data repository
#' maintained by the USFS AirFire team.
#'
#' Current year files loaded by this function are updated once per week.
#'
#' For the most recent data in the last 10 days, use \code{clarity_loadLatest()}.
#'
#' @note
#' The \code{QC_calibration} argument specifies the calibration/correction
#' algorithm used by Clarity for PM2.5 measurements.
#' See \href{https://www.clarity.io/blog/clarity-releases-v2-pm-global-calibration-model-with-significant-performance-improvements}{Clarity v2 clibration model}
#' It is recommended that users stick with the default calibration:
#' \code{"global PM2.5 v2.1"}. Specifying "all" will retrieve measurements
#' harvested before the introduction of \code{"global PM2.5 v2.1"} and are not
#' directly comparable with more recent measurements.
#'
#' @section Disclaimer:
#' Data archives accessed by this function are not maintained by Clarity and are
#' provided with no guarantee of completeness or future maintenance. They may be
#' removed at any time with no warning.
#'
#' @seealso \code{\link{clarity_loadLatest}}
#'
#' @examples
#' \dontrun{
#' library(AirMonitor)
#'
#' # Fail gracefully if any resources are not available
#' try({
#'
#' # 2025 Emigrant Fire
#' # See https://inciweb.wildfire.gov/incident-information/orwif-emigrant-fire
#'
#' # Clarity monitors in Oak Ridge, OR (-122.4670, 43.7473)
#' clarity_loadAnnual(2025) %>%
#'   monitor_filterByDistance(-122.4670, 43.7473, 10000) %>%
#'   monitor_collapse(deviceID = "Oak_Ridge") %>%
#'   monitor_filterDate(20250815, 20250915, timezone = "America/Los_Angeles") %>%
#'   monitor_dailyBarplot(main = "Clarity Monitors in Oak Ridge, Oregon")
#'
#' }, silent = FALSE)
#' }

clarity_loadAnnual <- function(
  year = NULL,
  archiveBaseUrl = paste0(
    "INSERT_BASE_HERE",
    "sensors/v3/PM2.5"
  ),
  archiveBaseDir = NULL,
  QC_negativeValues = c("zero", "na", "ignore"),
  QC_calibration = c("global PM2.5 v2.1", "all")
) {

  parameterName <- "PM2.5"


  MazamaCoreUtils::stopIfNull(year)
  MazamaCoreUtils::stopIfNull(parameterName)

  QC_negativeValues <- match.arg(QC_negativeValues)
  QC_calibration <- match.arg(QC_calibration)

  if ( (as.numeric(year) < 2025) && (QC_calibration == "global PM2.5 v2.1"))
    stop(paste0("No Clarity data using the 'v2.1' calibration is available before 2025"))

  if ( as.numeric(year) < 2023 )
    stop(paste0("No Clarity data is available before 2023"))

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
    dataUrl <- file.path(archiveBaseUrl, "clarity", year, "data")
  }

  if ( is.null(archiveBaseDir) ) {
    dataDir <- NULL
  } else {
    dataDir <- file.path(archiveBaseDir, "clarity", year, "data")
  }

  metaFileName <- sprintf("clarity_%s_%s_meta.rda", parameterName, year)
  dataFileName <- sprintf("clarity_%s_%s_data.rda", parameterName, year)

  meta <- MazamaCoreUtils::loadDataFile(metaFileName, dataUrl, dataDir)
  data <- MazamaCoreUtils::loadDataFile(dataFileName, dataUrl, dataDir)

  # Guarantee that 'meta' and 'data' match
  ids <- names(data)[-1]

  meta <-
    meta %>%
    dplyr::filter(.data$deviceDeploymentID %in% ids)

  # Guarantee presence of fullAQSID and postalCode
  if ( !"fullAQSID" %in% names(meta) ) meta$fullAQSID <- NA_character_
  if ( !"postalCode" %in% names(meta) ) meta$postalCode <- NA_character_

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

  # Handle negative values
  if ( QC_negativeValues == "zero" ) {
    monitor <- monitor_replaceValues(monitor, data < 0, 0)
  } else if ( QC_negativeValues == "na" ) {
    monitor <- monitor_replaceValues(monitor, data < 0, as.numeric(NA))
  }

  # Filter on QC_calibration
  if ( QC_calibration != "all") {
    monitor <-
      monitor %>%
      monitor_filter(.data$calibrationCategory == QC_calibration)
  }

  # Drop monitors with no data
  monitor <-
    monitor %>%
    monitor_dropEmpty()

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}

# ===== DEBUG ==================================================================

if ( FALSE ) {


  year <- 2024
  archiveBaseUrl <- "https://airfire-data-exports.s3.us-west-2.amazonaws.com/sensors/v3/PM2.5"
  archiveBaseDir <- NULL
  QC_negativeValues <- "zero"
  QC_calibration <- "global PM2.5 v2.1"


clarity_loadAnnual(2025) %>%
    monitor_filterByDistance(-122.4670, 43.7473, 10000) %>%
    monitor_collapse(deviceID = "Oak_Ridge") %>%
    monitor_filterDate(20250815, 20250915, timezone = "America/Los_Angeles") %>%
    monitor_dailyBarplot(main = "Clarity Monitors in Oak Ridge, Oregon")

}
