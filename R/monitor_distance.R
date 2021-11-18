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
#' @return Vector of of distances (meters) named by \code{deviceDeploymentID}.
#'
#' @examples
#' library(AirMonitor)
#'
#' PacNW <- example_88101
#'
#' # Walla Walla
#' lon <- -118.330278
#' lat <- 46.065
#'
#' distance <- monitor_distance(PacNW, lon, lat)
#' closestIndex <- which(distance == min(distance))
#'
#' print("Distances from Walla Walla")
#' sprintf(
#'   "%s: %d km",
#'   PacNW$meta$locationName,
#'   round(distance / 1000)
#' )

monitor_distance <- function(
  monitor = NULL,
  longitude = NULL,
  latitude = NULL,
  measure = c("geodesic", "haversine", "vincenty", "cheap")
) {

  # ----- Validate parameters --------------------------------------------------

  # NOTE:  Validate is handled by MazamaTimeSeries::mts_distance()

  # ----- Call MazamaTimeSeries function ---------------------------------------

  monitor <-
    MazamaTimeSeries::mts_distance(
      mts = monitor,
      longitude = longitude,
      latitude = latitude,
      measure = measure
    )

  class(monitor) <- union("mts_monitor", class(monitor))

  # ----- Return ---------------------------------------------------------------

  return(invisible(monitor))

}
