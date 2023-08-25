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
#' @note This function guarantees that only a single time series will be
#' associated with each \code{locationID} using the following logic:
#' \enumerate{
#' \item{AirNow data takes precedence over data from AIRSIS or WRCC}
#' \item{more recent data takes precedence over older data}
#' }
#' This relevant mostly for "temporary" monitors which may be replaced after they
#' are initially deployed. If you want access to all device deployments associated
#' with a specific \code{locationID}, you can use the provider specific functions:
#' \code{\link{airnow_loadDaily}},
#' \code{\link{airsis_loadDaily}} and
#' \code{\link{wrcc_loadDaily}}
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
    monitorList[["airnow"]] <-
      airnow_loadDaily(archiveBaseUrl, archiveBaseDir, QC_negativeValues, parameterName) %>%
      monitor_dropEmpty()
  }, silent = TRUE)

  try({
    monitorList[["airsis"]] <-
      airsis_loadDaily(archiveBaseUrl, archiveBaseDir, QC_negativeValues) %>%
      monitor_dropEmpty()
  }, silent = TRUE)

  try({
    monitorList[["wrcc"]] <-
      wrcc_loadDaily(archiveBaseUrl, archiveBaseDir, QC_negativeValues) %>%
      monitor_dropEmpty()
  }, silent = TRUE)

  # ----- Remove older deployments ---------------------------------------------

  for ( name in names(monitorList) ) {

    monitor <- monitorList[[name]]

    # Find locations with multiple deployments
    duplicateLocationIDs <-
      monitor$meta$locationID[duplicated(monitor$meta$locationID)] %>%
      unique()

    # Filter to include only locations with multiple deployments
    monitor <-
      monitor %>%
      monitor_filter(.data$locationID %in% duplicateLocationIDs)

    # Find last valid datum for each deployment (see monitor_getCurrentStatus.R)
    monitor$meta$lastValidIndex <-
      # Start with data
      monitor$data %>%
      # Ensure rows are arranged by datetime and then remove 'datetime'
      dplyr::arrange(.data$datetime) %>%
      dplyr::select(-.data$datetime) %>%
      # Find last non-NA index
      apply(2, function(x) { rev(which(!is.na(x)))[1] })

    # Find deployments to be removed
    deploymentList <- list()

    for (locationID in duplicateLocationIDs) {
      latestValid <-
        monitor$meta %>%
        dplyr::filter(.data$locationID == !!locationID) %>%
        dplyr::pull(.data$lastValidIndex) %>%
        max()

      deploymentList[[locationID]] <-
        monitor$meta %>%
        dplyr::filter(.data$locationID == !!locationID) %>%
        dplyr::filter(.data$lastValidIndex != !!latestValid) %>%
        dplyr::pull(.data$deviceDeploymentID)
    }

    deploymentsToRemove <- unlist(deploymentList)

    # Replace monitor object with only the most recent deployments
    deploymentsToRetain <-
      setdiff(monitorList[[name]]$meta$deviceDeploymentID, deploymentsToRemove)

    monitorList[[name]] <-
      monitorList[[name]] %>%
      monitor_select(deploymentsToRetain)

    # NOTE:  Some locations like the Rocky Mtn Fire Cache will have multiple
    # NOTE:  monitors all producing data at the same time. in this case, we
    # NOTE:  rely on dplyr::distinct() below to simply pick the first one.

  }

  # ----- Remove duplicate locations -------------------------------------------

  # NOTE:  Whenever we have multiple monitors reporting from the same location,
  # NOTE:  we always favor the data fom AirNow over AIRSIS and WRCC.
  # NOTE:  Because airnow comes first in monitorList, AirNow data
  # NOTE:  will be preferentially retained.

  monitor_all <-
    monitor_combine(monitorList)

  ids <-
    monitor_all$meta %>%
    dplyr::distinct(.data$locationID, .keep_all = TRUE) %>%
    dplyr::pull(.data$deviceDeploymentID)

  monitor <- monitor_all %>% monitor_select(ids)

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}
