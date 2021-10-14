#' @title Get time related information
#'
#' @description Calculate the local time at the target location, as well as
#' sunrise, sunset and solar noon times, and create several temporal masks.
#'
#' The returned dataframe will have as many rows as the length of the incoming
#' UTC \code{time} vector and will contain the following columns:
#'
#' \itemize{
#' \item{\code{localStdTime_UTC} -- UTC representation of local \strong{standard} time}
#' \item{\code{daylightSavings} -- logical mask = TRUE if daylight savings is in effect}
#' \item{\code{localTime} -- local clock time}
#' \item{\code{sunrise} -- time of sunrise on each localTime day}
#' \item{\code{sunset} -- time of sunset on each localTime day}
#' \item{\code{solarnoon} -- time of solar noon on each localTime day}
#' \item{\code{day} -- logical mask = TRUE between sunrise and sunset}
#' \item{\code{morning} -- logical mask = TRUE between sunrise and solarnoon}
#' \item{\code{afternoon} -- logical mask = TRUE between solarnoon and sunset}
#' \item{\code{night} -- logical mask = opposite of day}
#' }
#'
#' @details
#' While the \pkg{lubridate} package makes it easy to work in local timezones,
#' there is no easy way in R to work in "Local Standard Time" (LST) (\emph{i.e.
#' never shifting to daylight savings}) as is often required when working with
#' air quality data. US EPA regulations mandate that daily averages be calculated
#' based on LST.
#'
#'
#' The \code{localStdTime_UTC} column in the returned dataframe is primarily for
#' internal use and provides an important tool for creating LST daily averages
#' and LST axis labeling.
#'
#' @param time POSIXct vector with specified timezone,
#' @param longitude Longitude of the location of interest.
#' @param latitude Latitude of the location of interest.
#' @param timezone Olson timezone at the location of interest.
#'
#' @return A dataframe with times and masks.
#'
#' @importFrom rlang .data
#' @importFrom lubridate is.POSIXct
#' @export
#'
#' @examples
#' carmel <-
#'   Carmel_Valley %>%
#'   monitor_filterDate(20160801, 20160810)
#'
#' # Create timeInfo object for this monitor
#' ti <- timeInfo(
#'   carmel$data$datetime,
#'   carmel$meta$longitude,
#'   carmel$meta$latitude,
#'   carmel$meta$timezone
#' )
#'
#' # Subset the data based on day/night masks
#' data_day <- carmel$data[ti$day,]
#' data_night <- carmel$data[ti$night,]
#'
#' # Build two monitor objects
#' carmel_day <- list(meta = carmel$meta, data = data_day)
#' carmel_night <- list(meta = carmel$meta, data = data_night)
#'
#' # Plot them
#' monitor_timeseriesPlot(carmel_day, shadedNight = TRUE, pch = 8, col = 'goldenrod')
#' monitor_timeseriesPlot(carmel_night, pch = 16, col = 'darkblue', add = TRUE)

timeInfo <- function(
  time = NULL,
  longitude = NULL,
  latitude = NULL,
  timezone = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(time)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(timezone)

  if ( !lubridate::is.POSIXct(time) )
    stop(sprintf("time' must be of class POSIXct"))

  if ( !is.numeric(longitude) )
    stop(sprintf("'longitude' must be of class numeric"))

  if ( !is.numeric(latitude) )
    stop(sprintf("'latitude' must be of class numeric"))

  if ( !(timezone %in% base::OlsonNames()) )
    stop(sprintf("timezone = '%s' is not found in base::OlsonNames()", timezone))

  # ----- Solar times ----------------------------------------------------------

  # convert to local time
  localTime <- lubridate::with_tz(time, tzone = timezone)

  # sunriset reqires matrix or spatial object for input
  coords <- matrix(c(longitude, latitude), nrow = 1)

  # calculate sunrise, sunset, and solar noon times using fancy algorithm
  sunrise <- .maptools_sunriset(coords, localTime, direction = "sunrise", POSIXct.out = TRUE)
  sunset <- .maptools_sunriset(coords, localTime, direction = "sunset", POSIXct.out = TRUE)
  solarnoon <- .maptools_solarnoon(coords, localTime, POSIXct.out = TRUE)

  sunrise <- sunrise[,2] ; sunset <- sunset[,2] ; solarnoon <- solarnoon[,2]

  # create masks
  dayMask <- (localTime >= sunrise) & (localTime < sunset)
  nightMask <- !dayMask
  morningMask <- (localTime > sunrise) & (localTime <= solarnoon)
  afternoonMask <- (localTime > solarnoon) & (localTime <= sunset)

  # ----- localStandardTime_UTC ------------------------------------------------

  # NOTE:  The EPA defines regulatory daily averages as midnight-to-midnight
  # NOTE:  in local-standard-time-all-year. We add a column of data that
  # NOTE:  displays the proper clock time for LSTAY. This can then be used to
  # NOTE:  calculate, plot and label the EPA regulatory midnight-to-midnight
  # NOTE:  daily averages

  # Calculate the Local Standard Time offset
  Christmas_UTC <- lubridate::ymd_h("2019-12-25 00", tz = "UTC")
  Christmas_localTime <- lubridate::with_tz(Christmas_UTC, tzone = timezone)
  Christmas_localTime_UTC <- lubridate::force_tz(Christmas_localTime, tzone = "UTC")
  lst_offset <- as.numeric(difftime(Christmas_localTime_UTC, Christmas_UTC, units = "hours"))

  localStandardTime_UTC <- lubridate::with_tz(localTime, tzone = "UTC") +
    lst_offset * lubridate::dhours(1)

  # ----- Return ---------------------------------------------------------------

  # Assemble dataframe
  timeInfo <- data.frame(
    localStandardTime_UTC = localStandardTime_UTC,
    daylightSavings = lubridate::dst(localTime),
    localTime = localTime,
    sunrise = sunrise,
    sunset = sunset,
    solarnoon = solarnoon,
    day = dayMask,
    morning = morningMask,
    afternoon = afternoonMask,
    night = nightMask
  )

  return(timeInfo)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  Thompson_Falls <- monitor_load(2018110307, 2018110607,
                                 monitorIDs = "300890007_01")
  time <- Thompson_Falls$data$datetime
  timezone <- Thompson_Falls$meta$timezone
  longitude <- Thompson_Falls$meta$longitude
  latitude <- Thompson_Falls$meta$latitude
  timeInfo <- timeInfo(time, longitude, latitude, timezone)
  t(timeInfo[24:27,])

}



