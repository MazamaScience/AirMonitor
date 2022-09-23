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

  if ( !is.function(FUN) )
    stop("'FUN' is not a function.")

  if ( !is.logical(FUN(c(1:5,NA,6:10))) )
    stop("'FUN' does not return a logical value. Do you need to include 'na.rm = TRUE'?")

  # ----- Apply function -------------------------------------------------------

  # See https://dplyr.tidyverse.org/articles/colwise.html

  # Get dataBrick
  tbl <- monitor$data[,-1]

  # Apply function
  mask <-
    tbl %>%
    dplyr::summarize(dplyr::across(.cols = dplyr::everything(), FUN)) %>%
    as.logical()

  # Get deviceDeploymentIDs where FUN returns TRUE
  ids <- names(tbl)[mask]

  # Select those monitors
  monitor <- monitor %>% monitor_select(ids)

  # ----- Return ---------------------------------------------------------------

  return(invisible(monitor))

}
