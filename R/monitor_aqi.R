#' @export
#'
#' @title Calculate hourly NowCast-based AQI values
#'
#' @param monitor \emph{mts_monitor} object.
#' @param version Name of the type of nowcast algorithm to be used.
#' @param includeShortTerm Logical specifying whether to alcluate preliminary
#' NowCast values starting with the 2nd hour.
#' @param NAAQS Version of NAAQS levels to use. See Note.
#'
#' @return A modified \code{mts_monitor} object containing AQI values. (A list
#' with \code{meta} and \code{data} dataframes.)
#'
#' @description Nowcast and AQI algorithms are applied to the data in the
#' monitor object. A modified \code{mts_monitor} object is returned whre values
#' have been replaced with their Air Quality Index equivalents. See \link{monitor_nowcast}.
#'
#' @note
#' On February 7, 2024, EPA strengthened the National Ambient Air Quality
#' Standards for Particulate Matter (PM NAAQS) to protect millions of Americans
#' from harmful and costly health impacts, such as heart attacks and premature
#' death. Particle or soot pollution is one of the most dangerous forms of air
#' pollution, and an extensive body of science links it to a range of serious
#' and sometimes deadly illnesses. EPA is setting the level of the primary
#' (health-based) annual PM2.5 standard at 9.0 micrograms per cubic meter to
#' provide increased public health protection, consistent with the available
#' health science.
#' See \href{https://www.epa.gov/pm-pollution/final-reconsideration-national-ambient-air-quality-standards-particulate-matter-pm}{PM NAAQS update}.
#'
#' @references \url{https://en.wikipedia.org/wiki/Nowcast_(Air_Quality_Index)}
#' @references \url{https://www.airnow.gov/aqi/aqi-basics/}
#'

monitor_aqi <- function(
  monitor,
  version = c("pm", "pmAsian", "ozone"),
  includeShortTerm = FALSE,
  NAAQS = c("PM2.5_2024", "PM2.5")
) {

  parameterName <- "PM2.5"

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  version <- match.arg(version)
  includeShortTerm <- MazamaCoreUtils::setIfNull(includeShortTerm, FALSE)
  NAAQS = match.arg(NAAQS)

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

  # ----- AQI algorithm --------------------------------------------------------

  # Calculate NowCast
  monitor <-
    monitor %>%
    # NOTE: see https://forum.airnowtech.org/t/how-does-airnow-handle-negative-hourly-concentrations/143
    monitor_replaceValues(data < 0, 0) %>%
    monitor_nowcast(version = version, includeShortTerm = includeShortTerm)

  # pull out data-only for AQI calculation (i.e. drop 'datetime')
  dataBrick <- dplyr::select(monitor$data, -1)

  if ( parameterName == "PM2.5" || version == "pm" ) {
    digits <- 1
  } else {
    digits <- 0
  }

  dataBrick <- trunc(dataBrick*10^digits)/10^digits

  monitor$data[,-1] <- nowcast_to_aqi(dataBrick)

  # ----- Update meta ----------------------------------------------------------

  monitor$meta$pollutant <- "AQI"
  monitor$meta$units <- ""

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}

