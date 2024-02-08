#' @export
#'
#' @title Subset time series based on their position within an \emph{mts_monitor} object
#'
#' @param monitor \emph{mts_monitor} object.
#' @param n Number of rows of \code{monitor$meta} to select.
#'
#' @description An \emph{mts_monitor} object is reduced so as to contain only
#' the first or last \code{n} timeseries. These functions work similarly to
#' \code{\link[dplyr:slice_head]{dplyr::slice_head}} and
#' \code{\link[dplyr:slice_tail]{dplyr::slice_tail}}
#' but apply to both dataframes in the \emph{mts_monitor} object.
#'
#' This is primarily useful when the \emph{mts_monitor} object has been ordered
#' by a previous call to \code{\link{monitor_arrange}} or by some other means.
#'
#' \code{monitor_slice_head()} selects the first and \code{monitor_slice_tail()}
#' the last timeseries in the object.
#'
#' @return A subset of the incoming \emph{mts_monitor} time series object.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#'
#' @examples
#' library(AirMonitor)
#'
#' # Find lowest elevation sites
#' Camp_Fire %>%
#'   monitor_filter(!is.na(elevation)) %>%
#'   monitor_arrange(elevation) %>%
#'   monitor_slice_head(n = 5) %>%
#'   monitor_getMeta() %>%
#'   dplyr::select(elevation, locationName)
#'
#' # Find highest elevation sites
#' Camp_Fire %>%
#'   monitor_filterMeta(!is.na(elevation)) %>%
#'   monitor_arrange(elevation) %>%
#'   monitor_slice_tail(n = 5) %>%
#'   monitor_getMeta() %>%
#'   dplyr::select(elevation, locationName)
#'

#' @export
#' @rdname monitor_slice
monitor_slice_head <- function(
    monitor,
    n = 5
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  MazamaCoreUtils::stopIfNull(n)

  # NOTE:  Additional validation is handled by MazamaTimeSeries::mts_slice_head()

  # ----- Call MazamaTimeSeries function ---------------------------------------

  monitor <-
    MazamaTimeSeries::mts_slice_head(
      mts = monitor,
      n = n
    )

  class(monitor) <- union("mts_monitor", class(monitor))

  # ----- Return ---------------------------------------------------------------

  return(invisible(monitor))

}


#' @export
#' @rdname monitor_slice
monitor_slice_tail <- function(
    monitor,
    n = 5
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  MazamaCoreUtils::stopIfNull(n)

  # NOTE:  Additional validation is handled by MazamaTimeSeries::mts_slice_head()

  # ----- Call MazamaTimeSeries function ---------------------------------------

  monitor <-
    MazamaTimeSeries::mts_slice_tail(
      mts = monitor,
      n = n
    )

  class(monitor) <- union("mts_monitor", class(monitor))

  # ----- Return ---------------------------------------------------------------

  return(invisible(monitor))

}
