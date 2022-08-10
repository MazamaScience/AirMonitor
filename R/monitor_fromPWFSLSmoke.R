#' @export
#' @importFrom dplyr all_of
#'
#' @title Convert a ws_monitor object from the PWFSLSmoke package
#'
#' @param ws_monitor \emph{ws_monitor} object. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @return A \emph{mts_monitor} object.
#'
#' @description A \pkg{PWFSLSmoke} package \emph{ws_monitor} object is enhanced
#' and modified so that it becomes a valid \emph{mts_monitor} object. This is
#' a lossless operation and can be reversed with \code{monitor_toPWFSLSmoke()}.
#'
#'

monitor_fromPWFSLSmoke <- function(
  ws_monitor = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(ws_monitor)

  # ----- Create meta ----------------------------------------------------------

  # > print(names(ws_monitor$meta), width = 75)
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

  commonColumns <- intersect(names(ws_monitor$meta), coreMetadataNames)

  # > print(commonColumns, width = 75)
  # [1] "longitude"   "latitude"    "elevation"   "timezone"    "countryCode"
  # [6] "stateCode"   "countyName"

  missingColumns <- setdiff(coreMetadataNames, names(ws_monitor$meta))

  # > print(missingColumns, width = 75)
  #  [1] "deviceDeploymentID"    "deviceID"
  #  [3] "deviceType"            "deviceDescription"
  #  [5] "deviceExtra"           "pollutant"
  #  [7] "units"                 "dataIngestSource"
  #  [9] "dataIngestURL"         "dataIngestUnitID"
  # [11] "dataIngestExtra"       "dataIngestDescription"
  # [13] "locationID"            "locationName"
  # [15] "houseNumber"           "street"
  # [17] "city"                  "zip"
  # [19] "AQSID"                 "fullAQSID"

  meta <-
    ws_monitor$meta %>%

    # NOTE:  ws_monitor$meta$instrumentID has no useful information

    # Rename some columns
    dplyr::rename(
      deviceID = .data$monitorID,
      deviceType = .data$monitorType,
      locationName = .data$siteName,
      AQSID = .data$aqsID,
      dataIngestSource = .data$pwfslDataIngestSource,
      dataIngestUnitID = .data$telemetryUnitID
    ) %>%

    # Other core monitoring metadata
    dplyr::mutate(
      fullAQSID = .data$AQSID,
      locationID = MazamaCoreUtils::createLocationID(.data$longitude, .data$latitude),
      pollutant = "PM2.5",
      units = "UG/M3",
      deviceDescription = as.character(NA),
      deviceExtra = as.character(NA),
      dataIngestURL = as.character(NA),
      dataIngestExtra = as.character(NA),
      dataIngestDescription = as.character(NA),
      deploymentType = as.character(NA)
    )

  # Fix deviceID, deploymentType:
  #   for AirNow data, deviceID = monitorID minus the "_01"
  #   for AIRSIS/WRCC, deviceID = unitID

  mask <- (meta$pwfslDataIngestSource == "AIRNOW")
  meta$deviceID[mask] <- stringr::str_replace(meta$deviceID[mask], "_01$", "")
  # NOTE:  PWFSLSmoke AirNow data is assumed to be "Permanent"
  meta$deploymentType[mask] = "Permanent"

  mask <- (meta$pwfslDataIngestSource == "AIRSIS")
  meta$deviceID[mask] <- meta$instrumentID[mask]
  meta$deploymentType[mask] = "Temporary"

  mask <- (meta$pwfslDataIngestSource == "WRCC")
  meta$deviceID[mask] <- meta$instrumentID[mask]
  meta$deploymentType[mask] = "Temporary"

  # Other core location metadata
  meta <-
    meta %>%
    dplyr::mutate(
      deviceDeploymentID = paste0(.data$locationID, "_", .data$deviceID),
      houseNumber = as.character(NA),
      street = as.character(NA),
      city = as.character(NA),
      zip = as.character(NA)
    )

  # TODO:  We need an example object with the updated (2022-08-10) metadata
  # TODO:  so we can pull fields from that object rather than creating them
  # TODO:  manually.

  # airsis_loadLatest() %>% monitor_getMeta() %>% names() %>% paste0(collapse = '", "') %>% cat()

  desiredColumns <- c(
    "deviceDeploymentID",
    "deviceID",
    "deviceType",
    "deviceDescription",
    "deviceExtra",
    "pollutant",
    "units",
    "dataIngestSource",
    "dataIngestURL",
    "dataIngestUnitID",
    "dataIngestExtra",
    "dataIngestDescription",
    "locationID",
    "locationName",
    "longitude",
    "latitude",
    "elevation",
    "countryCode",
    "stateCode",
    "countyName",
    "timezone",
    "houseNumber",
    "street",
    "city",
    "zip",
    "AQSID",
    "fullAQSID",
    ###"airnow_stationID", "airnow_parameterName", "airnow_monitorType", "airnow_siteCode", "airnow_status", "airnow_agencyID", "airnow_agencyName", "airnow_EPARegion", "airnow_GMTOffsetHours", "airnow_CBSA_ID", "airnow_CBSA_Name", "airnow_stateAQSCode", "airnow_countyAQSCode", "airnow_MSAName", "address", "airnow_countryCode", "airnow_stateCode", "airnow_timezone", "airnow_houseNumber", "airnow_street", "airnow_city", "airnow_zip", "airsis_Alias", "airsis_dataFormat", "airsis_provider", "airsis_unitID",
    "deploymentType"
  )

  # Reorganize the columns
  newColumns <- intersect(names(meta), desiredColumns)

  meta <-
    meta %>%
    dplyr::select(all_of(newColumns))

  # ----- Create data ----------------------------------------------------------

  # Guarantee columns are in the correct order

  oldColumnNames <- c('datetime', meta$deviceID)
  newColumnNames <- c('datetime', meta$deviceDeploymentID)

  data <-
    ws_monitor$data %>%
    dplyr::select(all_of(oldColumnNames))

  names(data) <- newColumnNames

  # ----- Create mts_monitor ---------------------------------------------------

  monitor <- list(meta = meta, data = data)

  monitor <- structure(monitor, class = c("mts_monitor", "mts", class(monitor)))

  # ----- Return ---------------------------------------------------------------

  monitor_check(monitor)

  return(monitor)

}

# ===== DEBUG ==================================================================

if ( FALSE ) {

  library(AirMonitor)

  ws_monitor <- PWFSLSmoke::monitor_loadLatest()

  monitor <- monitor_fromPWFSLSmoke(ws_monitor)

  monitor %>%
    monitor_filter(stateCode == "IA") %>%
    monitor_timeseriesPlot()


}
