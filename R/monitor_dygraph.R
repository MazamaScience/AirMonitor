#' @export
#'
#' @title Create Interactive Time Series Plot
#'
#' @param monitor \emph{mts_monitor} object.
#' @param title Title text.
#' @param ylab Title for the y axis
#' @param rollPeriod Rolling mean to be applied to the data.
#' @param showLegend Logical to toggle display of the legend.
#'
#' @description This function creates interactive graphs that will be displayed
#' in RStudio's 'Viewer' tab.
#'
#' @return Initiates the interactive dygraph plot in RStudio's 'Viewer' tab.
#'
#' @examples
#' \dontrun{
#' library(AirMonitor)
#'
#' # Multiple monitors
#' Camp_Fire %>%
#'   monitor_filter(countyName == "Alameda") %>%
#'   monitor_dygraph()
#' }


monitor_dygraph <- function(
  monitor,
  title = "title",
  ylab = "PM2.5 Concentration",
  rollPeriod = 1,
  showLegend = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  monitor_check(monitor)

  if ( monitor_isEmpty(monitor) )
    stop("monitor object has no data")

  # Set timezone
  timezone <- monitor_bestTimezone(monitor)

  # Simplify access to variables
  datetime <- monitor$data$datetime

  # Create an xts from all data columns except the first which is 'datetime'
  timeseriesData <- xts::xts(monitor$data[, -1], datetime, tzone = timezone)

  names(timeseriesData) <- monitor$meta$locationName

  show <- ifelse(showLegend, "always", "never")

  # Create dygraph
  dygraphs::dygraph(timeseriesData, main = title, ylab = ylab) %>%
    dygraphs::dyOptions(useDataTimezone = TRUE) %>% # Always show local time
    dygraphs::dyLegend(show = show, width = 250, labelsSeparateLines = TRUE) %>%
    ###dygraphs::dyRangeSelector(dateWindow = dateWindow) %>%
    dygraphs::dyRoller(rollPeriod = rollPeriod)

}
