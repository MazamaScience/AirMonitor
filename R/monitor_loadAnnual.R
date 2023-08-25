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
#' @note This function guarantees that only a single time series will be
#' associated with each \code{locationID} using the following logic:
#' \enumerate{
#' \item{AirNow data takes precedence over data from AIRSIS or WRCC}
#' \item{more recent data takes precedence over older data}
#' }
#' This relevant mostly for "temporary" monitors which may be replaced after they
#' are initially deployed. If you want access to all device deployments associated
#' with a specific \code{locationID}, you can use the provider specific functions:
#' \code{\link{airnow_loadAnnual}},
#' \code{\link{airsis_loadAnnual}} and
#' \code{\link{wrcc_loadAnnual}}
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
        monitorList[["airnow"]] <-
          airnow_loadAnnual(year, archiveBaseUrl, archiveBaseDir, QC_negativeValues, parameterName) %>%
          monitor_dropEmpty()
      }, silent = TRUE)

    } else {

      # EPA AQS 88101 + 88502 files
      try({
        monitorList[["epa_aqs"]] <-
          epa_aqs_loadAnnual(year, archiveBaseUrl, archiveBaseDir, QC_negativeValues, parameterCode = "PM2.5") %>%
          monitor_dropEmpty()
      }, silent = TRUE)

    }

  } else {

    # EPA AQS 88101 + 88502 files
    if ( year >= firstEpa88101Year ) {
      try({
        monitorList[["epa_aqs"]] <-
          epa_aqs_loadAnnual(year, archiveBaseUrl, archiveBaseDir, QC_negativeValues, parameterCode = "PM2.5") %>%
          monitor_dropEmpty()
      }, silent = TRUE)
    }

  }

  # AIRSIS annual files
  if ( year >= firstAirsisYear ) {
    try({
      monitorList[["airsis"]] <-
        airsis_loadAnnual(year, archiveBaseUrl, archiveBaseDir, QC_negativeValues) %>%
        monitor_dropEmpty()
    }, silent = TRUE)
  }

  # WRCC annual files
  if ( year >= firstWrccYear ) {
    try({
      monitorList[["wrcc"]] <-
        wrcc_loadAnnual(year, archiveBaseUrl, archiveBaseDir, QC_negativeValues) %>%
        monitor_dropEmpty()
    }, silent = TRUE)
  }

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

  # NOTE:  This applies only to AirNow data, not EPA AQS data.
  # NOTE:
  # NOTE:  Whenever we have multiple monitors reporting from the same location,
  # NOTE:  we always favor the data fom AirNow over AIRSIS and WRCC.
  # NOTE:  Because airnow comes first in monitor_combine() above, AirNow data
  # NOTE:  will be preferentially retained.

  monitor_all <-
    monitor_combine(monitorList)

  ids <-
    monitor_all$meta %>%
    dplyr::distinct(.data$locationID, .keep_all = TRUE) %>%
    dplyr::pull(.data$deviceDeploymentID)

  monitor <- monitor_select(monitor_all, ids)

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}
