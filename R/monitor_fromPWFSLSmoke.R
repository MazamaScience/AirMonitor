#' @export
#' @importFrom dplyr all_of
#'
#' @title Convert a ws_monitor object from the PWFSLSmoke package
#'
#' @param ws_monitor \emph{ws_monitor} object
#'
#' @return A \emph{mts_monitor} object.
#'
#' @description A **PWFSLSMoke** package \emph{ws_monitor} object is enhanced
#' and modified so that it becomes a valid \emph{mts_monitor} object. This is
#' a lossless operation and can be reversed with \code{monitor_toPWFSLSmoke()}.
#'
#'

monitor_fromPWFSLSmoke <- function(
  ws_monitor = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(ws_monitor)

  # ----- Harmonize data -------------------------------------------------------

  # * meta -----

  # NOTE:  Here are the commonalities:
  # NOTE:    > intersect(names(ws_monitor$meta), names(example_88101$meta))
  # NOTE:      [1] "longitude"   "latitude"    "elevation"   "timezone"    "countryCode"
  # NOTE:      [6] "stateCode"

  commonColumns <- intersect(names(ws_monitor$meta), coreMetadataNames)
  missingColumns <- setdiff(coreMetadataNames, names(ws_monitor$meta))

  # > print(commonColumns, width = 75)
  #  [1] "longitude"   "latitude"    "elevation"   "timezone"    "countryCode"
  #  [6] "stateCode"   "countyName"
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

  meta <-
    ws_monitor$meta %>%

    # Rename some columns
    dplyr::rename(
      deviceID = .data$monitorID,
      deviceType = .data$monitorType,
      locationName = .data$siteName,
      ###county = .data$countyName,
      dataIngestSource = .data$pwfslDataIngestSource,
      dataIngestUnitID = .data$telemetryUnitID
    ) %>%

    # Other core monitoring metadata
    dplyr::mutate(
      locationID = MazamaCoreUtils::createLocationID(.data$longitude, .data$latitude),
      pollutant = "PM2.5",
      units = "UG/M3",
      deviceDescription = as.character(NA),
      deviceExtra = as.character(NA),
      dataIngestURL = as.character(NA),
      dataIngestExtra = as.character(NA),
      dataIngestDescription = as.character(NA)
    )

  # Fix deviceID:
  #   for AirNow data, deviceID = monitorID minus the "_01"
  #   for AIRSIS/WRCC, deviceID = unitID

  mask <- (meta$pwfslDataIngestSource == "AIRNOW")
  meta$deviceID[mask] <- stringr::str_replace(meta$deviceID[mask], "_01$", "")
  mask <- (meta$pwfslDataIngestSource == "AIRSIS")
  meta$deviceID[mask] <- meta$instrumentID[mask]
  mask <- (meta$pwfslDataIngestSource == "WRCC")
  meta$deviceID[mask] <- meta$instrumentID[mask]

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

  # Reorganize the columns
  pwfslColumns <- setdiff(names(meta), coreMetadataNames)
  newColumns <- c(coreMetadataNames, pwfslColumns)

  meta <-
    meta %>%
    dplyr::select(all_of(newColumns))

  # * data -----

  # Guarantee columns are in the correct order

  oldColumnNames <- c('datetime', meta$deviceID)
  newColumnNames <- c('datetime', meta$deviceDeploymentID)

  data <-
    ws_monitor$data %>%
    dplyr::select(all_of(oldColumnNames))

  names(data) <- newColumnNames

  # ----- Create new mts_monitor -----------------------------------------------

  monitor <- list(meta = meta, data = data)

  monitor <- structure(monitor, class = c("mts_monitor", "mts", class(monitor)))

  # ----- Return ---------------------------------------------------------------

  monitor_check(monitor)

  return(monitor)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  library(AirMonitor)

  ws_monitor <- PWFSLSmoke::monitor_loadLatest()

  monitor <- monitor_fromPWFSLSmoke(ws_monitor)


}
