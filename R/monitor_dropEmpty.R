#' @export
#' @importFrom rlang .data
#'
#' @title Drop device deployments with all missing data
#'
#' @param monitor \emph{mts_monitor} object. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @description The incoming \emph{mts_monitor} object is subset to retain
#' only time series with valid data.
#'
#' @return A subset of the incoming \code{mts_monitor}. (A list with
#' \code{meta} and \code{data} dataframes.)
#'

monitor_dropEmpty <- function(
  monitor
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

  if ( monitor_isEmpty(monitor) )
    stop("Parameter 'monitor' has no data.")

  # Remove any duplicate data records
  monitor <- monitor_distinct(monitor)

  # ----- Select monitors with data --------------------------------------------

  any_finite <- function(x) any(is.finite(x))

  # https://stackoverflow.com/questions/62459736/how-do-i-use-tidyselect-where-in-a-custom-package
  monitor$data <-
    monitor$data %>%
    dplyr::select(tidyselect::vars_select_helpers$where(any_finite))

  ids <- names(monitor$data)[-1]

  monitor <-
    monitor %>%
    MazamaTimeSeries::mts_filterMeta(.data$deviceDeploymentID %in% ids)

  class(monitor) <- union("mts_monitor", class(monitor))

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}
