#' @export
#'
#' @title Move an \emph{mts_monitor} object to a new location
#'
#' @param monitor \emph{mts_monitor} object.
#' @param id \code{deviceDeploymentID} for a single time series found in \code{monitor}.
#' (Optional if \code{monitor} contains only a single time series.)
#' @param longitude New longitude of the time series.
#' @param latitude New latitude of the time series.
#' @param algorithm Algorithm to use -- either \code{"geohash"} or \code{"digest"}.
#' @param precision \code{precision} argument used when encoding with \code{"geohash"}.
#'
#' @return A \emph{mts_monitor} object representing a single time series. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @description
#' Changes the location associated with an existing time series. This will update
#' the following fields in \code{monitor$meta}: \code{longitude}, \code{latitude},
#' \code{locationID} and \code{deviceDeploymentID} as well as the column name of this
#' time series in \code{monitor$data}.
#'
#' @details
#' If another time series exists at the the specified location, \code{monitor_combine()}
#' will be used to join them into a single time series. Combination will be performed
#' with \code{replaceMeta = TRUE, overlapStrategy = "replace all"} to ensure that
#' data and metadata associated with the \emph{later} time series take precedence.
#'
#' A typical use case would involve a monitor whose location metadata was
#' update/corrected after data collection has already started. This will result
#' in two separate time series that need to be combined.
#'
#' @note
#' Arguments \code{algorithm} and \code{precision} are passed on to
#' \code{\link[MazamaCoreUtils]{createLocationID}} so that the new \code{locationID}
#' will match those found in \code{monitor}. If \code{algorithm = NULL}, the
#' \code{algorithm} and \code{precision} will be chosen to match those used to
#' create \code{mon$meta$locationID}.
#'
#' @examples
#' library(AirMonitor)
#'
#' # Move Carmel Vallely monitor over a bit
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

monitor_move <- function(
  monitor,
  id = NULL,
  longitude = NULL,
  latitude = NULL,
  algorithm = NULL,
  precision = 10
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  MazamaCoreUtils::stopIfNull(id)
  MazamaCoreUtils::validateLonLat(longitude, latitude)
  algorithm <- match.arg(algorithm)
  precision <- MazamaCoreUtils::setIfNull(precision, 10)

  if ( monitor_isEmpty(monitor) )
    stop("monitor is empty")

  if ( !id %in% monitor$meta$deviceDeploymentID )
    stop(sprintf("%s is not found in monitor", id))

  # ----- Move selected monitor ------------------------------------------------

  mon <- monitor %>% monitor_select(id)

  old_locationID <- mon$meta$locationID

  # Match algorithm and precision to existing locationID
  if ( is.null(algorithm) ) {
    len <- stringr::str_length(old_locationID)
    if ( len == 16 ) {
      algorithm <- "digest"
    } else {
      algorithm <- "geohash"
      precision <- len
    }
  }

  # Create metadata for the new location
  locationID <-
    MazamaCoreUtils::createLocationID(longitude, latitude, algorithm, precision)

  deviceDeploymentID <- sprintf("%s_%s", locationID, mon$meta$deviceID)

  # Update metadata
  mon$meta$longitude <- longitude
  mon$meta$latitude <- latitude
  mon$meta$locationID <- locationID
  mon$meta$deviceDeploymentID <- deviceDeploymentID

  names(mon$data) <- c("datetime", deviceDeploymentID)

  monitor_check(mon)

  # ----- Combine with monitor at new location ---------------------------------

  monitor <-
    monitor %>%
    # Remove timeseries associated with incoming id
    monitor_filter(deviceDeploymentID != id) %>%
    # Combine moved monitor with existing timeseries
    monitor_combine(
      mon,
      replaceMeta = TRUE,
      overlapStrategy = "replace na"
    )

  # ----- Return ---------------------------------------------------------------

  return(invisible(monitor))

}
