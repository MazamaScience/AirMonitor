#' @title Get time related information for a monitor
#'
#' @description Calculate the local time for a monitor, as well as
#' sunrise, sunset and solar noon times, and create several temporal masks.
#'
#' The returned dataframe will have as many rows as the length of the incoming
#' UTC \code{time} vector and will contain the following columns:
#'
#' \itemize{
#' \item{\code{localStdTime_UTC} -- UTC representation of local \strong{standard} time}
#' \item{\code{daylightSavings} -- logical mask = TRUE if daylight savings is in effect}
#' \item{\code{localTime} -- local clock time}
#' \item{\code{sunrise} -- time of sunrise on each localTime day}
#' \item{\code{sunset} -- time of sunset on each localTime day}
#' \item{\code{solarnoon} -- time of solar noon on each localTime day}
#' \item{\code{day} -- logical mask = TRUE between sunrise and sunset}
#' \item{\code{morning} -- logical mask = TRUE between sunrise and solarnoon}
#' \item{\code{afternoon} -- logical mask = TRUE between solarnoon and sunset}
#' \item{\code{night} -- logical mask = opposite of day}
#' }
#'
#' @details
#' While the \pkg{lubridate} package makes it easy to work in local timezones,
#' there is no easy way in R to work in "Local Standard Time" (LST) (\emph{i.e.
#' never shifting to daylight savings}) as is often required when working with
#' air quality data. US EPA regulations mandate that daily averages be calculated
#' based on LST.
#'
#' The \code{localStdTime_UTC} is primarily for use internally and provides
#' an important tool for creating LST daily averages and LST axis labeling.
#'
#' @param monitor \emph{mts_monitor} object.
#' @param id \code{deviceDeploymentID} used to select a single time
#' series found in \code{monitor}. -- optional if \code{monitor} only has one
#' time series.
#'
#' @return A dataframe with times and masks.
#'
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' library(AirMonitor)
#'
#' carmel <-
#'   Carmel_Valley %>%
#'   monitor_filterDate(20160801, 20160810)
#'
#' # Create timeInfo object for this monitor
#' ti <- monitor_timeInfo(carmel)
#'
#' # Subset the data based on day/night masks
#' data_day <- carmel$data[ti$day,]
#' data_night <- carmel$data[ti$night,]
#'
#' # Build two monitor objects
#' carmel_day <- list(meta = carmel$meta, data = data_day)
#' carmel_night <- list(meta = carmel$meta, data = data_night)
#'
#' # Plot them
#' carmel_day %>%
#'   monitor_timeseriesPlot(
#'     pch = 8,
#'     col = "goldenrod",
#'     shadedNight = TRUE
#'   )
#'
#' carmel_night %>%
#'   monitor_timeseriesPlot(
#'     add = TRUE,
#'     pch = 16,
#'     col = "darkblue"
#'   )

monitor_timeInfo <- function(
  monitor = NULL,
  id = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  monitor_check(monitor)

  if ( nrow(monitor$meta) == 1 ) {
    deviceDeploymentID <- monitor$meta$deviceDeploymentID[1]
  } else {
    if ( is.null(deviceDeploymentID) ) {
      stop("'deviceDeploymentID' must be specified if more than one monitor is present")
    } else if ( !deviceDeploymentID %in% monitor$meta$deviceDeploymentID ) {
      stop(sprintf("deviceDeploymentID $s is not found in 'monitor'", deviceDeploymentID))
    }
    monitor <-
      monitor_filterMeta(.data$deviceDeploymentID == !!deviceDeploymentID)
  }

  # ----- Return ---------------------------------------------------------------

  timeInfo <-
    MazamaTimeSeries::timeInfo(
      monitor$data$datetime,
      monitor$meta$longitude,
      monitor$meta$latitude,
      monitor$meta$timezone
    )

  return(timeInfo)

}



