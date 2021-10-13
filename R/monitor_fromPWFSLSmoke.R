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

  # > commonColumns
  # [1] "longitude"   "latitude"    "elevation"   "timezone"    "countryCode"
  # [6] "stateCode"
  # > missingColumns
  # [1] "deviceDeploymentID" "deviceID"           "pollutant"
  # [4] "units"              "locationID"         "locationName"

  meta <-
    ws_monitor$meta %>%

    # Add locationID, deviceID, pollutant and units
    dplyr::mutate(
      locationID = MazamaCoreUtils::createLocationID(.data$longitude, .data$latitude),
      deviceID = .data$monitorID,
      pollutant = "PM2.5",
      units = "ug/m3"
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

  # Other core metadata
  meta <-
    meta %>%
    dplyr::mutate(
      deviceDeploymentID = paste0(.data$locationID, "_", .data$deviceID),
      locationName = .data$siteName,
      county = .data$countyName,
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

  oldColumnNames <- c('datetime', meta$monitorID)
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

  ws_monitor <- PWFSLSmoke::monitor_loadLatest()

  monitor <- monitor_fromPWFSLSmoke(ws_monitor)


}
