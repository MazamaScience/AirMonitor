#' @export
#' @importFrom rlang .data
#'
#' @title Data-based subsetting of time series within an \emph{mts_monitor} object.
#'
#' @param monitor \emph{mts_monitor} object.
#' @param FUN A function applied to time series data that returns TRUE or FALSE.
#'
#' @description
#' Subsetting of \code{monitor} acts similarly to \code{tidyselect::where()} working on
#' \code{monitor$data}. The returned \emph{mts_monitor} object will contain only
#' those time series where \code{FUN} applied to the time series data returns \code{TRUE}.
#'
#' @return A subset of the incoming \emph{mts_monitor} object. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @seealso \link{monitor_select}
#'
#' @examples
#' library(AirMonitor)
#'
#' # Show all Camp_Fire locations
#' Camp_Fire$meta$locationName
#'
#' # Use package US_AQI data for HAZARDOUS
#' name <- US_AQI$names_eng[6]
#' threshold <- US_AQI$breaks_PM2.5[6]
#'
#' # Find HAZARDOUS locations
#' worst_sites <-
#'   Camp_Fire %>%
#'   monitor_selectWhere(
#'     function(x) { any(x >= threshold, na.rm = TRUE) }
#'   )
#'
#' # Show the worst locations
#' worst_sites$meta$locationName
#'

monitor_selectWhere <- function(
  monitor,
  FUN
) {

  # NOTE:  Validate is handled by MazamaTimeSeries::mts_selectWhere()

  # ----- Call MazamaTimeSeries function ---------------------------------------

  monitor <-
    MazamaTimeSeries::mts_selectWhere(
      mts = monitor,
      FUN = FUN
    )

  class(monitor) <- union("mts_monitor", class(monitor))

  # ----- Return ---------------------------------------------------------------

  return(invisible(monitor))

}
