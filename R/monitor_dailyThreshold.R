#' @export
#'
#' @title Daily counts of values at or above a threshold
#'
#' @param monitor \emph{mts_monitor} object.
#' @param threshold AQI level name (e.g. \code{"unhealthy"}) or numerical
#' threshold at or above which a measurement is counted.
#' @param na.rm Logical value indicating whether NA values should be ignored.
#' @param minHours Minimum number of valid hourly records per day required to
#' calculate statistics. Days with fewer valid records will be assigned \code{NA}.
#' @param dayBoundary Treatment of daylight savings time:  "clock" uses daylight
#' savings time as defined in the local timezone, "LST" uses "local standard time"
#' all year round.
#'
#' @return A \emph{mts_monitor} object containing daily counts of hours at or above
#' a threshold value..
#'
#' @description
#' Calculates the number of hours per day each monitor in \code{monitor} was
#' at or above a given threshold.
#'
#' Because the returned \emph{mts_monitor} object is defined on a daily axis in a
#' specific time zone, it is important that the incoming \code{monitor} contain
#' timeseries associated with a single time zone.
#'
#' @note
#' When \code{dayBoundary = "clock"}, the returned \code{monitor$data$datetime}
#' time axis will be defined in the local timezone (not "UTC") with days defined
#' by midnight as it appears on a clock in that timezone. The transition from
#' DST to standard time will result in a 23 hour day and standard to DST in a
#' 25 hour day.
#'
#' When \code{dayBoundary = "LST"}, the returned \code{monitor$data$datetime}
#' time axis will be defined in "UTC" with times as they \emph{appear} in standard
#' time in the local timezone. These days will be one hour off from clock
#' time during DST but every day will consist of 24 hours.
#'
#' @examples
#' Carmel_Valley %>%
#'   monitor_dailyThreshold("Moderate") %>%
#'   monitor_extractData()
#'
#' Carmel_Valley %>%
#'   monitor_dailyThreshold("Unhealthy") %>%
#'   monitor_extractData()
#'

monitor_dailyThreshold <- function(
  monitor = NULL,
  threshold = NULL,
  na.rm = TRUE,
  minHours = 18,
  dayBoundary = c("clock", "LST")
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  MazamaCoreUtils::stopIfNull(threshold)
  na.rm <- MazamaCoreUtils::setIfNull(na.rm, TRUE)
  MazamaCoreUtils::stopIfNull(minHours)
  dayBoundary <- match.arg(dayBoundary)

  if ( length(unique(monitor$meta$timezone)) > 1 )
    stop("'monitor' has muliple timezones")

  # Check if official AQI level name is provided
  if ( typeof(threshold) == "character" ) {

    if ( !tolower(threshold) %in% tolower(US_AQI$names_eng) )
      stop(sprintf("'%s' is not a recognized AQI level. Please use one from US_AQI$names_eng.", threshold))

    # > US_AQI$breaks_PM2.5
    # [1]  -Inf  12.0  35.5  55.5 150.5 250.5   Inf
    breaks <- US_AQI$breaks_PM2.5
    breaks[1] <- 0
    index <- which(tolower(US_AQI$names_eng) == tolower(threshold))
    threshold <- breaks[index]

  }

  # ----- Create threshold count -----------------------------------------------

  # Threshold function
  myFUN <- function(
    x,
    threshold,
    na.rm = TRUE
  ) {
    return(sum(x >= threshold, na.rm = na.rm))
  }

  # Use monitor_dailyStatistic to calculate counts
  overThreshold <- monitor_dailyStatistic(
    monitor = monitor,
    FUN = myFUN,
    na.rm = na.rm,
    threshold = threshold,
    minHours = minHours,
    dayBoundary = dayBoundary
  )

  # ----- Return ---------------------------------------------------------------

  return(overThreshold)

}