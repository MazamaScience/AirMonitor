#' @export
#'
#' @title Replace \emph{mts_monitor} data with another value
#'
#' @param monitor \emph{mts_monitor} object.
#' @param filter R expression used to identify values for replacement.
#' @param value Numeric replacement value.
#'
#' @description Use an R expression to identify values for replacement.
#'
#' The R expression given in \code{filter} is used to identify elements
#' in \code{monitor$data} that should be replaced.  The \code{datetime} column
#' will be retained unmodified. Typical usage would include
#'
#' \enumerate{
#' \item{replacing negative values with 0}
#' \item{replacing unreasonably high values with \code{NA}}
#' }
#'
#' Expressions should use \code{data} for the left hand side of the comparison.
#'
#' @return A modified \code{mts_monitor} object. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @examples
#' library(AirMonitor)
#'
#' wa <- monitor_filterMeta(NW_Megafires, stateCode == 'WA')
#' any(wa$data < 5, na.rm = TRUE)
#'
#' wa_zero <- monitor_replaceValues(wa, data < 5, 5)
#' any(wa_zero$data < 5, na.rm = TRUE)

monitor_replaceValues <- function(
  monitor = NULL,
  filter = NULL,
  value = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  MazamaCoreUtils::stopIfNull(value)

  if ( monitor_isEmpty(monitor) )
    stop("'monitor' has no data")

  # Remove any duplicate data records
  monitor <- monitor_distinct(monitor)

  # NOTE:  Test this with: condition_call <- substitute(data < 0)

  # Create a "condition call" -- basically, an expression that isn't run yet.
  condition_call <- substitute(filter)
  filterString <- paste(as.character(condition_call)[2],
                        as.character(condition_call)[1],
                        as.character(condition_call)[3])

  # NOTE:  Example condition_call:
  # NOTE:  > as.character(condition_call)
  # NOTE:  [1] "<"    "data" "0"

  if ( !any(stringr::str_detect(filterString, 'data')) )
    stop( sprintf("bad filter: \"%s\". Try something like \"data < 0\".", filterString) )

  # ----- Replace data ---------------------------------------------------------

  # Create a data-only tibble by omitting the first 'datetime' column
  data <-
    monitor$data %>%
    dplyr::select(-.data$datetime)

  # Find places where condition is true
  dataMask <- eval(condition_call)

  # NOTE:  Below is a previous version which passes a second argument to eval.
  #
  # # Use FUN to create a mask
  # FUN <- function(list) { eval(condition_call, data.frame(data = list)) }
  # dataMask <- apply(data, 2, FUN)
  # dataMask <- replace(dataMask, is.na(dataMask), FALSE)

  # Replace matching data with value
  data[dataMask] <- value

  # Replace monitor$data data columns with new data
  monitor$data[,-1] <- data

  # ----- Return -------------------------------------------------------------

  return( monitor )

}
