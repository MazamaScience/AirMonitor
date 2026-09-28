#' @export
#'
#' @title Move an \emph{mts_monitor} object to a new location
#'
#' @param monitor \emph{mts_monitor} object.
#' @param id \code{deviceDeploymentID} for a single time series found in
#' \code{monitor}. Optional if \code{monitor} contains only a single time series.
#' @param longitude New longitude of the time series.
#' @param latitude New latitude of the time series.
#' @param precision \code{precision} argument used when creating the geohash-based
#' \code{locationID}.
#'
#' @return A \emph{mts_monitor} object. (A list with \code{meta} and \code{data}
#' dataframes.)
#'
#' @description
#' Changes the location associated with an existing time series. This will update
#' the following fields in \code{monitor$meta}: \code{longitude}, \code{latitude},
#' \code{locationID} and \code{deviceDeploymentID}, as well as the column name of
#' this time series in \code{monitor$data}.
#'
#' @details
#' New locations are assigned geohash-based \code{locationID} values.
#'
#' If another time series exists with the same geohash-based location identifier,
#' \code{monitor_combine()} will be used to join them into a single time series.
#' Combination will be performed with \code{replaceMeta = TRUE} and
#' \code{overlapStrategy = "replace na"} so that data and metadata associated
#' with the moved time series take precedence where appropriate.
#'
#' A typical use case would involve a monitor whose location metadata was
#' updated or corrected after data collection had already started. This can
#' result in two separate time series that need to be combined.
#'
#' @note
#' Legacy monitors may contain digest-based \code{locationID} values. New
#' locations are always assigned geohash-based \code{locationID} values because
#' digest-based location identifiers are no longer supported by
#' \pkg{MazamaCoreUtils}.
#'
#' As a result, \code{monitor_combine()} may not recognize a legacy
#' digest-based location and a geohash-based location as representing the same
#' physical location.
#'
#' @examples
#' library(AirMonitor)
#'
#' # Move Carmel Valley monitor over a bit
#'
#' names(Carmel_Valley$data)
#' Carmel_Valley$meta %>%
#'   dplyr::select(longitude, latitude, locationID, deviceDeploymentID) %>%
#'   dplyr::glimpse()
#'
#' moved_monitor <- monitor_move(
#'   Carmel_Valley,
#'   id = Carmel_Valley$meta$deviceDeploymentID,
#'   longitude = Carmel_Valley$meta$longitude + 0.001,
#'   latitude = Carmel_Valley$meta$latitude + 0.001
#' )
#'
#' names(moved_monitor$data)
#' moved_monitor$meta %>%
#'   dplyr::select(longitude, latitude, locationID, deviceDeploymentID) %>%
#'   dplyr::glimpse()
#'

monitor_move <- function(
    monitor,
    id = NULL,
    longitude = NULL,
    latitude = NULL,
    precision = 10
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  MazamaCoreUtils::validateLonLat(longitude, latitude)
  precision <- MazamaCoreUtils::setIfNull(precision, 10)

  if ( monitor_isEmpty(monitor) )
    stop("monitor is empty")

  # If only one time series is present, id is optional
  if ( is.null(id) ) {
    if ( nrow(monitor$meta) == 1 ) {
      id <- monitor$meta$deviceDeploymentID
    } else {
      stop(
        "Parameter 'id' must be specified when monitor contains multiple time series."
      )
    }
  }

  if ( length(id) != 1 )
    stop("Parameter 'id' must identify a single time series.")

  if ( !id %in% monitor$meta$deviceDeploymentID )
    stop(sprintf("%s is not found in monitor", id))

  # ----- Move selected monitor ------------------------------------------------

  mon <- monitor %>%
    monitor_select(id)

  # Create metadata for the new location.
  # NOTE: Legacy digest-based locationIDs are no longer created.
  locationID <-
    MazamaCoreUtils::createLocationID(
      longitude = longitude,
      latitude = latitude,
      algorithm = "geohash",
      precision = precision
    )

  deviceDeploymentID <- sprintf("%s_%s", locationID, mon$meta$deviceID)

  # Update metadata
  mon$meta$longitude <- longitude
  mon$meta$latitude <- latitude
  mon$meta$locationID <- locationID
  mon$meta$deviceDeploymentID <- deviceDeploymentID

  # Update data column name to match the new deviceDeploymentID
  names(mon$data) <- c("datetime", deviceDeploymentID)

  monitor_check(mon)

  # ----- Combine with monitor at new location ---------------------------------

  monitor <-
    monitor %>%
    # Remove time series associated with incoming id
    monitor_filter(deviceDeploymentID != id) %>%
    # Combine moved monitor with existing time series
    monitor_combine(
      mon,
      replaceMeta = TRUE,
      overlapStrategy = "replace na"
    )

  # ----- Return ---------------------------------------------------------------

  return(invisible(monitor))

}
