#' @export
#' @importFrom rlang .data
#'
#' @title Subset and reorder time series within an \emph{mts_monitor} object
#'
#' @param monitor \emph{mts_monitor} object.
#' @param id Vector of \code{deviceDeploymentIDs}.
#'
#' @description
#' This function acts similarly to \code{dplyr::select()} working on
#' \code{monitor$data}. The returned \emph{mts_monitor} object will contain only
#' those time series identified by \code{id} in the order specified.
#'
#' This can be helpful when using faceted plot functions based on \pkg{ggplot}
#' such as those found in the \pkg{AirMonitorPlots} package.
#'
#' @return A reordered (subset) of the incoming \emph{mts_monitor} object. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @seealso \link{monitor_filterMeta}
#'

monitor_select <- function(
  monitor,
  id
) {

  # ----- Validate parameters --------------------------------------------------

  # NOTE:  Validate is handled by MazamaTimeSeries::mts_select()

  # ----- Call MazamaTimeSeries function ---------------------------------------

  monitor <-
    MazamaTimeSeries::mts_select(
      mts = monitor,
      deviceDeploymentID = id
    )

  class(monitor) <- union("mts_monitor", class(monitor))

  # ----- Return ---------------------------------------------------------------

  return(invisible(monitor))

}

# ===== Alias ==================================================================

# TODO:  Add examples to the alias

#' @rdname monitor_select
#' @export
monitor_reorder <- monitor_select
