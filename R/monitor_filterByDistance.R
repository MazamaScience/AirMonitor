#' @export
#'
#' @title Filter by distance from a target location
#'
#' @param monitor \emph{mts_monitor} object.
#' @param longitude Target longitude.
#' @param latitude Target.
#' @param radius Distance (m) of radius defining a target area.
#' @param count Number of time series to return.
#' @param addToMeta Logical specifying whether to add \code{distanceFromTarget}
#' as a field in \code{monitor$meta}.
#'
#' @return A \emph{mts_monitor} object with monitors near a location.
#'
#' @description Filters the \code{monitor} argument to include only those time series
#' located within a certain radius of a target location. If no time series fall
#' within the specified \code{radius}, an empty \emph{mts_monitor} object will
#' be returned.
#'
#' When \code{count} is used, a \emph{mts_monitor} object is
#' created containing \strong{up to} \code{count} time series, ordered by
#' increasing distance from the target location. Note that the number
#' of monitors returned may be less than the specified \code{count} value if
#' fewer than \code{count} time series are found within the target area.
#'
#' @note The returned \emph{mts_monitor} will have an extra \code{distance}.  (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @examples
#' library(AirMonitor)
#'
#' # Walla Walla
#' longitude <- -118.330278
#' latitude <- 46.065
#'
#' Walla_Walla_monitors <-
#'   NW_Megafires %>%
#'   monitor_filterByDistance(
#'     longitude = -118.330,
#'     latitude = 46.065,
#'     radius = 50000,     # 50 km
#'     addToMeta = TRUE
#'   )
#'
#' Walla_Walla_monitors %>%
#'   monitor_getMeta() %>%
#'   dplyr::select(c("locationName", "distanceFromTarget"))
#'

monitor_filterByDistance <- function(
  monitor,
  longitude = NULL,
  latitude = NULL,
  radius = 50,
  count = NULL,
  addToMeta = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  MazamaCoreUtils::validateLonLat(longitude, latitude)

  if ( monitor_isEmpty(monitor) )
    stop("monitor is empty")

  # ----- Filter by distance ---------------------------------------------------

  # * Create distance mask -----

  distance <- monitor_getDistance(monitor, longitude, latitude)
  distanceMask <- distance <= radius

  # * Return if empty -----

  if ( !(any(distanceMask)) ) {

    monitor <-
      monitor %>%
      monitor_filter(.data$deviceDeploymentID == "DONT FIND ME")
    return(monitor)

  }

  # * Apply distance filter -----

  ids <- monitor$meta$deviceDeploymentID[distanceMask]
  monitor <- monitor %>% monitor_select(ids)

  # Update the distance vector for the new subset of monitors
  distance <- distance[distanceMask]

  if ( addToMeta )
    monitor$meta$distanceFromTarget <- round(as.numeric(distance))

  # * Handle count -----

  if ( is.null(count) )
    count <- nrow(monitor$meta)

  # NOTE:  When using count, return monitors in distance order and make sure
  # NOTE:  that the distances are also subset and returned in distance order.

  # Find the 'count' closest monitors
  closestIndices <- order(distance)[1:count]

  # Reorder
  ids <- ids[closestIndices]
  monitor <- monitor %>% monitor_select(ids)

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}

