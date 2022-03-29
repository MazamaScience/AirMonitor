#' @export
#'
#' @title Apply a function to \emph{mts_monitor} time series
#'
#' @param monitor \emph{mts_monitor} object.
#' @param FUN Function used to modify time series.
#' @param ... Additional arguments to be passed to \code{FUN}.
#'
#' @description
#' This function works similarly to \code{dplyr::mutate()} and applies
#' \code{FUN} to each time series found in \code{monitor$data}. \code{FUN} must
#' be a function that accepts a numeric vector as its first argument and returns
#' a vector of the same length.
#'
#'
#' @return A modified \code{mts_monitor} object. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @examples
#' library(AirMonitor)
#'
#' Carmel_Valley %>%
#'   monitor_filterDatetime(2016080207, 2016080212) %>%
#'   monitor_toCSV(includeMeta = FALSE) %>%
#'   cat()
#'
#' Carmel_Valley %>%
#'   monitor_filterDatetime(2016080207, 2016080212) %>%
#'   monitor_mutate(function(x) { return(x / 2) }) %>%
#'   monitor_toCSV(includeMeta = FALSE) %>%
#'   cat()
#'

monitor_mutate <- function(
  monitor = NULL,
  FUN = NULL,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  MazamaCoreUtils::stopIfNull(FUN)

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

  if ( monitor_isEmpty(monitor) )
    stop("Parameter 'monitor' has no data.")

  # ----- Apply function -------------------------------------------------------

  dataBrick <- base::apply(
    dplyr::select(monitor$data, -1),
    2,
    FUN,
    ...
  )

  monitor$data <- cbind(dplyr::select(monitor$data, 1), dataBrick)

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}

