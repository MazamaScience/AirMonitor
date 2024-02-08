#' @export
#'
#' @title Extract a column of metadata or data
#'
#' @param monitor \emph{mts_monitor} object.
#' @param var A variable name found in the \code{meta} or \code{data}
#' dataframe of the incoming \emph{mts_monitor} time series object.
#'
#' @description
#' This function acts similarly to \code{\link[dplyr]{pull}} working on
#' \code{monitor$meta} or \code{monitor$data}. Data are returned as a simple array.
#' Data are pulled from whichever dataframe contains \code{var}.
#'
#' @return An array of values.
#'
#' @examples
#' library(AirMonitor)
#'
#' # Metadata
#' Camp_Fire %>%
#'   monitor_pull("deploymentType") %>%
#'   table()
#'
#' # Data for a specific ID
#' Camp_Fire %>%
#'   monitor_dailyStatistic(mean) %>%
#'   monitor_pull("6bbab08e3786ef66_840060450006") %>%
#'   round(0)
#'
#' # Associated dates
#' Camp_Fire %>%
#'   monitor_dailyStatistic(mean) %>%
#'   monitor_pull("datetime")
#'

monitor_pull <- function(
    monitor = NULL,
    var = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  MazamaCoreUtils::stopIfNull(var)

  # NOTE:  Additional validation is handled by MazamaTimeSeries::mts_pull()

  # ----- Call MazamaTimeSeries function ---------------------------------------

  values <-
    MazamaTimeSeries::mts_pull(
      mts = monitor,
      var = var
    )

  # ----- Return ---------------------------------------------------------------

  return(values)

}


