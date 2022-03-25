#' @export
#'
#' @title Calculate distances from \emph{mts_monitor} locations to a location of interest
#'
#' @param monitor \emph{mts_monitor} object.
#' @param longitude Longitude of the location of interest.
#' @param latitude Latitude of the location of interest.
#' @param measure One of "geodesic", "haversine" "vincenty", or "cheap".
#'
#' @description
#' This function returns the distances (meters) between \code{monitor} locations
#' and a location of interest. These distances can be used to create a
#' mask identifying monitors within a certain radius of the location of interest.
#'
#' @note The measure \code{"cheap"} may be used to speed things up depending on
#' the spatial scale being considered. Distances calculated with
#' \code{measure = "cheap"} will vary by a few meters compared with those
#' calculated using \code{measure = "geodesic"}.
#'
#' @return Named vector of distances (meters) with each distance identified
#' by \code{deviceDeploymentID}.
#'
#' @examples
#' library(AirMonitor)
#'
#' # Walla Walla
#' longitude <- -118.3302
#' latitude <- 46.065
#'
#' distance <- monitor_getDistance(NW_Megafires, longitude, latitude)
#' closestIndex <- which(distance == min(distance))
#'
#' # Distance in meters
#' distance[closestIndex]
#'
#' # Monitor metadata
#' str(NW_Megafires$meta[closestIndex,])
#'

monitor_getDistance <- function(
  monitor = NULL,
  longitude = NULL,
  latitude = NULL,
  measure = c("geodesic", "haversine", "vincenty", "cheap")
) {

  # ----- Validate parameters --------------------------------------------------

  # NOTE:  Validate is handled by MazamaTimeSeries::mts_getDistance()

  # ----- Call MazamaTimeSeries function ---------------------------------------

  distance <-
    MazamaTimeSeries::mts_getDistance(
      mts = monitor,
      longitude = longitude,
      latitude = latitude,
      measure = measure
    )

  # ----- Return ---------------------------------------------------------------

  return(distance)

}
