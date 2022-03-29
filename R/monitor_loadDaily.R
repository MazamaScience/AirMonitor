#' @export
#'
#' @title Load daily monitoring data from all sources
#'
#' @param archiveBaseUrl Base URL for monitoring v2 data files.
#' @param archiveBaseDir Local base directory for monitoring v2 data files.
#' @param QC_negativeValues Type of QC to apply to negative values.
#'
#' @return A \emph{mts_monitor} object with PM2.5 monitoring data. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @description Combine daily data from AirNow, AIRSIS and WRCC:
#'
#' If \code{archiveDataDir} is defined, data will be loaded from this local
#' archive. Otherwise, data will be loaded from the monitoring data repository
#' maintained by the USFS AirFire team.
#'
#' The files loaded by this function are updated once per day and
#' contain data for the previous 45 days.
#'
#' For the most recent data in the last 10 days, use \code{monitor_loadLatest()}.
#'
#' For data extended more than 45 days into the past, use \code{monitor_load()}.
#'
#' @note The AirNow data stream contains data may also be available from AIRSIS
#' and WRCC. This can be detected by looking at the `locationID` associated with
#' each time series. Wherever multiple time series share a `locationID`, the
#' time series from AirNow or WRCC are removed so that each location is represented
#' by a single time series coming from AirNow.
#'
# #' @seealso \code{\link{monitor_load}}
#' @seealso \code{\link{monitor_loadAnnual}}
#' @seealso \code{\link{monitor_loadLatest}}
#' @examples
#' \dontrun{
#' library(AirMonitor)
#' # Fail gracefully if any resources are not available
#' try({
#'
#' monitor_loadDaily() %>%
#'   monitor_filter(stateCode %in% CONUS) %>%
#'   monitor_leaflet()
#'
#' }, silent = FALSE)
#' }

monitor_loadDaily <- function(
  archiveBaseUrl = paste0(
    "https://airfire-data-exports.s3.us-west-2.amazonaws.com/",
    "monitoring/v2"
  ),
  archiveBaseDir = NULL,
  QC_negativeValues = c("zero", "na", "ignore")
) {

  parameterName <- "PM2.5"

  # ----- Validate parameters --------------------------------------------------

  QC_negativeValues <- match.arg(QC_negativeValues)

  if ( is.null(archiveBaseUrl) && is.null(archiveBaseDir) )
    stop("one of 'archiveBaseUrl' or 'archiveBaseDir' must be defined")

  # ----- Load data ------------------------------------------------------------

  monitorList <- list()

  try({
    monitorList[["airnow"]] <- airnow_loadDaily(archiveBaseUrl, archiveBaseDir, QC_negativeValues, parameterName)
  }, silent = TRUE)

  try({
    monitorList[["airsis"]] <- airsis_loadDaily(archiveBaseUrl, archiveBaseDir, QC_negativeValues)
  }, silent = TRUE)

  try({
    monitorList[["wrcc"]] <- wrcc_loadDaily(archiveBaseUrl, archiveBaseDir, QC_negativeValues)
  }, silent = TRUE)

  monitor_all <-
    monitor_combine(monitorList) %>%
    monitor_dropEmpty()

  # ----- Remove duplicate locations -------------------------------------------

  # NOTE:  Whenever we have multiple monitors reporting from the same location,
  # NOTE:  we always favor the data fom AirNow over AIRSIS and WRCC.
  # NOTE:  Because airnow comes first in monitor_combine() above, AirNow data
  # NOTE:  will be preferentially retained.

  ids <-
    monitor_all$meta %>%
    dplyr::distinct(.data$locationID, .keep_all = TRUE) %>%
    dplyr::pull(.data$deviceDeploymentID)

  monitor <- monitor_select(monitor_all, ids)

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}
