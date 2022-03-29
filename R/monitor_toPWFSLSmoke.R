#' @export
#' @importFrom dplyr all_of
#'
#' @title Convert a mts_monitor object to a ws_monitor object for the PWFSLSmoke package
#'
#' @param monitor \emph{mts_monitor} object
#'
#' @return A \pkg{PWFSLSmoke} \emph{ws_monitor} object. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @description A \emph{mts_monitor} object is modified so that it becomes
#' a \pkg{PWFSLSmoke} package \emph{ws_monitor} object. While some information
#' will be lost, this operation can be reversed with \code{monitor_fromPWFSLSmoke()}.
#'
#' @note In order to avoid duplicated \code{monitorID} values in the returned
#' \emph{ws_monitor} object, the full \code{deviceDeploymentID} will be used
#' as the \code{monitorID}.
#'

monitor_toPWFSLSmoke <- function(
  monitor = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  monitor_check(monitor)

  # ----- Create meta ----------------------------------------------------------

  # > names(PWFSLSmoke::Carmel_Valley$meta) %>% print(width = 75)
  # [1] "monitorID"             "longitude"
  # [3] "latitude"              "elevation"
  # [5] "timezone"              "countryCode"
  # [7] "stateCode"             "siteName"
  # [9] "agencyName"            "countyName"
  # [11] "msaName"               "monitorType"
  # [13] "siteID"                "instrumentID"
  # [15] "aqsID"                 "pwfslID"
  # [17] "pwfslDataIngestSource" "telemetryAggregator"
  # [19] "telemetryUnitID"

  newColumns <- c(
    "monitorID",
    "longitude",
    "latitude",
    "elevation",
    "timezone",
    "countryCode",
    "stateCode",
    "siteName",
    "agencyName",
    "countyName",
    "msaName",
    "monitorType",
    "siteID",
    "instrumentID",
    "aqsID",
    "pwfslID",
    "pwfslDataIngestSource",
    "telemetryAggregator",
    "telemetryUnitID"
  )

  commonColumns <- intersect(newColumns, AirMonitor::coreMetadataNames)

  # > print(commonColumns, width = 75)
  # [1] "longitude"   "latitude"    "elevation"   "timezone"    "countryCode"
  # [6] "stateCode"   "countyName"

  missingColumns <- setdiff(newColumns, AirMonitor::coreMetadataNames)

  # > print(missingColumns, width = 75)
  # [1] "monitorID"             "siteName"
  # [3] "agencyName"            "msaName"
  # [5] "monitorType"           "siteID"
  # [7] "instrumentID"          "aqsID"
  # [9] "pwfslID"               "pwfslDataIngestSource"
  # [11] "telemetryAggregator"   "telemetryUnitID"

  # Available columns
  # > print(coreMetadataNames, width = 75)
  # [1] "deviceDeploymentID"    "deviceID"
  # [3] "deviceType"            "deviceDescription"
  # [5] "deviceExtra"           "pollutant"
  # [7] "units"                 "dataIngestSource"
  # [9] "dataIngestURL"         "dataIngestUnitID"
  # [11] "dataIngestExtra"       "dataIngestDescription"
  # [13] "locationID"            "locationName"
  # [15] "longitude"             "latitude"
  # [17] "elevation"             "countryCode"
  # [19] "stateCode"             "countyName"
  # [21] "timezone"              "houseNumber"
  # [23] "street"                "city"
  # [25] "zip"                   "AQSID"

  meta <-

    monitor$meta %>%

    # Add other metadata
    dplyr::mutate(
      monitorID = .data$deviceDeploymentID,
      siteName = .data$locationName,
      agencyName = as.character(NA),
      msaName = as.character(NA),
      monitorType = .data$deviceType,
      siteID = as.character(NA),
      instrumentID = as.character(NA),
      aqsID = .data$AQSID,
      pwfslID = as.character(NA),
      pwfslDataIngestSource = toupper(.data$dataIngestSource),
      telemetryAggregator = as.character(NA),
      telemetryUnitID = as.character(NA)
    )

  # Fix siteID:
  #   for AirNow data, siteID = aqsID

  mask <- (meta$pwfslDataIngestSource == "AIRNOW")
  meta$siteID[mask] <- meta$aqsID[mask]

  # Reorganize the columns
  meta <-
    meta %>%
    dplyr::select(all_of(newColumns)) %>%
    as.data.frame()

  rownames(meta) <- meta$monitorID

  # ----- Create data ----------------------------------------------------------

  # Guarantee columns are in the correct order

  dataColumns <- c('datetime', meta$monitorID)

  data <-
    monitor$data %>%
    dplyr::select(all_of(dataColumns)) %>%
    as.data.frame()

  # ----- Create ws_monitor ----------------------------------------------------

  ws_monitor <- list(meta = meta, data = data)
  ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))

  # ----- Return ---------------------------------------------------------------

  return(ws_monitor)

}

# ===== DEBUG ==================================================================

if ( FALSE ) {

  library(AirMonitor)

  monitor <-
    airnow_loadLatest(
      archiveBaseUrl = "https://data-monitoring1.airfire.org/monitoring-v2",
      archiveBaseDir = NULL,
      QC_negativeValues = "zero",
      parameterName = "PM2.5"
    )

  ws_monitor <- monitor_toPWFSLSmoke(monitor)

  ws_monitor %>%
    PWFSLSmoke::monitor_subset(stateCodes = "IA") %>%
    AirMonitorPlots::monitor_ggDailyHourlyBarplot()

}
