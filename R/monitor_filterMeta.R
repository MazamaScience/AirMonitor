#' @export
#'
#' @title General purpose metadata filtering for \emph{mts_monitor} objects
#'
#' @param monitor \emph{mts_monitor} object.
#' @param ... Logical predicates defined in terms of the variables in
#' \code{monitor$meta}.
#'
#' @description A generalized metadata filter for \emph{mts_monitor} objects to
#' choose cases where conditions are true.  Multiple conditions are
#' combined with \code{&} or separated by a comma. Only rows where the condition
#' evaluates to TRUE are kept. Rows of \code{monitor$meta} where the condition
#' evaluates to \code{NA} are dropped. Associated olumns of \code{monitor$data}
#' are also dropped for internal consistency in the returned \emph{mts_monitor}
#' object.
#'
#' \code{monitor_filter()} is an alias for \code{monitor_filterMeta()}.
#'
#' @note Filtering is done on variables in \code{monitor$meta}.
#'
#' @return A subset of the incoming \code{mts_monitor}. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @seealso \link{monitor_filterDate}
#' @seealso \link{monitor_filterDatetime}
#'
#' @examples
#' library(AirMonitor)
#'
#' # Filter based on countyName field
#' Camp_Fire %>%
#'   monitor_filter(countyName == "Alameda") %>%
#'   monitor_timeseriesPlot(main = "All Alameda County Monitors")
#'
#' # Filter combining two fields
#' Camp_Fire %>%
#'   monitor_filter(latitude > 39.5, longitude > -121.5) %>%
#'   monitor_pull("locationName")
#'
#' # Filter using string matching
#' Camp_Fire %>%
#'   monitor_filter(stringr::str_detect(locationName, "^San")) %>%
#'   monitor_pull("locationName")
#'

monitor_filterMeta <- function(
  monitor,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  # A little involved to catch the case where the user forgets to pass in 'monitor'

  result <- try({
    if ( !monitor_isValid(monitor) )
      stop("First argument is not a valid 'mts_monitor' object.")
  }, silent = TRUE)

  if ( class(result) %in% "try-error" ) {
    err_msg <- geterrmessage()
    if ( stringr::str_detect(err_msg, "object .* not found") ) {
      stop(paste0(err_msg, "\n(Did you forget to pass in the 'monitor' object?)"))
    }
  }

  # ----- Call MazamaTimeSeries function ---------------------------------------

  monitor <- MazamaTimeSeries::mts_filterMeta(monitor, ...)
  class(monitor) <- union("mts_monitor", class(monitor))

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}

# ===== Alias ==================================================================

# TODO:  Add examples to the alias

#' @rdname monitor_filterMeta
#' @export
monitor_filter <- monitor_filterMeta
