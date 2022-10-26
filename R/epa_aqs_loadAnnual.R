#' @export
#' @importFrom dplyr across
#'
#' @title Load annual AirNow monitoring data
#'
#' @param year Year [YYYY].
#' @param archiveBaseUrl Base URL for monitoring v2 data files.
#' @param archiveBaseDir Local base directory for monitoring v2 data files.
#' @param QC_negativeValues Type of QC to apply to negative values.
#' @param parameterCode One of the EPA AQS criteria parameter codes.
#'
#' @return A \emph{mts_monitor} object with EPA AQS data. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @description Loads pre-generated .rda files containing hourly AirNow data.
#'
#' If \code{archiveDataDir} is defined, data will be loaded from this local
#' archive. Otherwise, data will be loaded from the monitoring data repository
#' maintained by the USFS AirFire team.
#'
#' The files loaded by this function contain a single year's worth of data.
#'
#' Pre-processed AirNow exists for the following parameter codes:
#' \enumerate{
#' \item{88101 -- PM2.5 FRM/FEM Mass}
#' \item{88502 -- PM2.5 non FRM/FEM Mass}
#' }
#'
#' Specifying \code{parameterCode = "PM2.5"} will merge records from both
#' sources.
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
#' epa_aqs_loadAnnual(2015) \%>\%
#'   monitor_filter(stateCode == "WA") \%>\%
#'   monitor_filterDate(20150724, 20150907) \%>\%
#'   monitor_dailyStatistic() \%>\%
#'   monitor_timeseriesPlot(
#'     main = "Washington 2015 -- AirNow Daily Average PM2.5"
#'   )
#'
#' }, silent = FALSE)
#' }


epa_aqs_loadAnnual <- function(
  year = NULL,
  archiveBaseUrl = paste0(
    "https://airfire-data-exports.s3.us-west-2.amazonaws.com/",
    "monitoring/v2"
  ),
  archiveBaseDir = NULL,
  QC_negativeValues = c("zero", "na", "ignore"),
  parameterCode = c("PM2.5", "88101", "88502")
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(year)

  QC_negativeValues <- match.arg(QC_negativeValues)

  if ( is.null(archiveBaseUrl) && is.null(archiveBaseDir) )
    stop("one of 'archiveBaseUrl' or 'archiveBaseDir' must be defined")

  parameterCode <- match.arg(parameterCode)

  if ( parameterCode == "PM2.5" ) {
    parameterCodes <- c("88101", "88502")
  } else {
    parameterCodes <- parameterCode
  }

  # ----- Load data ------------------------------------------------------------

  # Create file name and path according to the AirMonitorIngest scheme

  if ( is.null(archiveBaseUrl) ) {
    dataUrl <- NULL
  } else {
    dataUrl <- file.path(archiveBaseUrl, "epa_aqs", year, "data")
  }

  if ( is.null(archiveBaseDir) ) {
    dataDir <- NULL
  } else {
    dataDir <- file.path(archiveBaseDir, "epa_aqs", year, "data")
  }

  monitorList <- list()

  for ( parameterCode in parameterCodes ) {

    result <- try({

      suppressWarnings({

        metaFileName <- sprintf("epa_aqs_%s_%s_meta.rda", parameterCode, year)
        dataFileName <- sprintf("epa_aqs_%s_%s_data.rda", parameterCode, year)

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

        monitorList[[parameterCode]] <- monitor
      })

    }, silent = TRUE)

  }

  # Test for data
  if ( length(monitorList) == 0 ) {
    if ( is.null(dataDir) ) {
      err_msg <- sprintf("no data could be loaded from dataUrl: %s\n\nDid you mean to specify dataDir?", dataUrl)
    } else {
      err_msg <- sprintf("no data could be loaded from dataDir: %s\n\nDid you mean to specify dataDir?", dataDir)
    }
    stop(err_msg)
  }

  if ( length(monitorList) > 1 ) {

    # NOTE:  Combine with "replace na" to handle cases where identical
    # NOTE:  deviceDeplomentIDs represent a "temporary" monitor from 88502 being
    # NOTE:  replaced with a "permanent" monitor from 88101.
    monitor <-
      monitor_combine(
        monitorList,
        replaceMeta = TRUE,
        overlapStrategy = "replace na"
      )

  }

  # ----- Apply QC -------------------------------------------------------------

  if ( QC_negativeValues == "zero" ) {

    monitor <- monitor_replaceValues(monitor, data < 0, 0)

  } else if ( QC_negativeValues == "na" ) {

    monitor <- monitor_replaceValues(monitor, data < 0, as.numeric(NA))

  }

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}

