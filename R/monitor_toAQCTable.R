#' @export
#' @importFrom rlang .data
#' @importFrom dplyr across everything na_if
#'
#' @title Convert monitor data into an AQI category table
#'
#' @param monitor \emph{mts_monitor} object.
#' @param NAAQS Version of NAAQS levels to use. See Note.
#' @param siteIdentifier Metadata column used to identify sites or a character
#' vector with site identifiers.
#'
#' @description Creates a table of AQI category vs monitoring site with a count
#' of the number of times each AQI category was experienced at each site. The
#' count will be a count of hours or days depending on averaging period of
#' the incoming \code{monitor} object.
#'
#' When \code{siteIdentifier} is used, the identifiers must be in the same
#' order as \code{monitor$meta}.
#'
#' @return Table of AQI category counts.
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
#' @examples
#' library(AirMonitor)
#'
#' # Lane County, Oregon AQSIDs all begin with "41039"
#' LaneCounty <-
#'   NW_Megafires %>%
#'   monitor_filter(stringr::str_detect(AQSID, '^41039')) %>%
#'   monitor_filterDate(20150801, 20150901)
#'
#' # Count of hours each site spent in each AQ category in August
#' LaneCounty %>%
#'   monitor_toAQCTable()
#'
#' # Count of days each site spent in each AQ
#' LaneCounty %>%
#'   monitor_dailyStatistic(mean) %>%
#'   monitor_toAQCTable()
#'
#' # Count of days each site spent in each AQ (simplified names)
#' siteNames <- c(
#'   "Eugene 1", "Eugene 2", "Eugene 3",
#'   "Springfield", "Oakridge", "Cottage Grove"
#' )
#' LaneCounty %>%
#'   monitor_dailyStatistic(mean) %>%
#'   monitor_toAQCTable(siteIdentifier = siteNames)
#'
#' # Count of days at each AQ level with the new, 2024 NAAQS
#' LaneCounty %>%
#'   monitor_dailyStatistic(mean) %>%
#'   monitor_toAQCTable(NAAQS = "PM2.5_2024")
#'
#'

monitor_toAQCTable <- function(
  monitor,
  NAAQS = c("PM2.5", "PM2.5_2024"),
  siteIdentifier = "locationName"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  NAAQS = match.arg(NAAQS)

  if ( !monitor_isValid(monitor) )
    stop("Parameter 'monitor' is not a valid 'mts_monitor' object.")

  if ( monitor_isEmpty(monitor) )
    stop("Parameter 'monitor' has no data.")

  pollutant <- unique(monitor$meta$pollutant)

  if ( length(pollutant) > 1 ) {
    pollutantString <- paste(pollutant, collapse = ", ")
    stop(sprintf("Monitor object contains multiple pollutants: %", pollutantString))
  }

  if ( length(siteIdentifier) == 1 ) {
    if ( !siteIdentifier %in% names(monitor$meta) ) {
      stop(sprintf("siteIdentifier '%s' is not found in monitor$meta", siteIdentifier))
    } else {
      siteNames <- monitor$meta[[siteIdentifier]]
    }
  } else {
    if ( length(siteIdentifier) != nrow(monitor$meta) ) {
      stop(sprintf("siteIdentifier array length: %d does not match montitor$meta rows: %d",
           length(siteIdentifier), nrow(monitor$meta)))
    } else {
      siteNames <- siteIdentifier
    }
  }

  # ----- Create table ---------------------------------------------------------

  # Get AQC matrix (rows = datetime, cols = site, cells = PM2.5)
  aqcMatrix <-
    monitor %>%
      aqiCategories(
        pollutant,
        NAAQS
      )

  # NOTE:  aqiCategories() can (intentionally) return a vector
  if ( "integer" %in% class(aqcMatrix)) {
    aqcMatrix <- as.matrix(aqcMatrix, ncol = 1)
  }

  # Create the empty counts table (rows = AQC, cols = site, cells = count)
  countsTable <- data.frame(row.names = c(US_AQI$names_eng, "Missing"))

  for ( i in 1:ncol(aqcMatrix) ) {
    # AQC counts per site
    aqcCounts <-
      aqcMatrix[,i] %>%
      factor(levels = 1:6, labels = US_AQI$names_eng, exclude = NA) %>%
      table(useNA = "always") %>%
      as.numeric()
    # Add another column of data named with siteIdentifier
    countsTable[[siteNames[i]]] = aqcCounts
  }

  # Create AQC table (rows = site, cols = AQC, cells = count)
  aqcTable <- t(countsTable)

  # ---- Return ----------------------------------------------------------------

  return(aqcTable)

}
