#' @export
#'
#' @title Load annual EPA AQS monitoring data
#'
#' @param year Desired year (integer or character representing YYYY).
#' @param parameterCode EPA AQS Parameter Code.
#' @param baseUrl Base URL for annual EPA AQS data files.
#' @param baseDir Local base directory for annual EPA AQS data files.
#'
#' @return A \emph{mts_monitor} object with EPA AQS data.
#'
#' @description Loads pre-generated .RData files containing annual
#' EPA data.
#'
#' EPA parameter codes include:
#' \enumerate{
#' \item{44201}{ -- Ozone}
# #' \item{42401}{ -- SO2}
#' \item{42101}{ -- CO}
# #' \item{42602}{ -- NO2}
#' \item{88101}{ -- PM2.5 FRM/FEM Mass (begins in 2008)}
#' \item{88502}{ -- PM2.5 non FRM/FEM Mass (begins in 1998)}
# #' \item{81102}{ -- PM10 Mass}
# #' \item{SPEC}{ -- PM2.5 Speciation}
# #' \item{WIND}{ -- Wind}
# #' \item{TEMP}{ -- Temperature}
# #' \item{PRESS}{ -- Barometric Pressure}
# #' \item{RH_DP}{ -- RH and dewpoint}
# #' \item{HAPS}{ -- HAPs}
# #' \item{VOCS}{ -- VOCs}
# #' \item{NONOxNOy}
#' }
#'

epa_aqs_loadAnnual <- function(
  year = NULL,
  parameterCode = NULL,
  baseUrl = NULL,
  baseDir = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(year)
  MazamaCoreUtils::stopIfNull(parameterCode)

  if ( is.null(baseUrl) && is.null(baseDir) )
    stop("one of 'baseUrl' or 'baseDir' must be defined")

  # Parameter code
  validParameterCodes <- c(
    "44201",
    # "42401",
    "42101",
    # "42602",
    "88101",
    "88502"
    # "81102",
    # "SPEC",
    # "WIND",
    # "TEMP",
    # "PRESS",
    # "RH_DP",
    # "HAPS",
    # "VOCS",
    # "NONOxNOy"
  )

  parameterCode <- as.character(parameterCode)
  if ( !parameterCode %in% validParameterCodes ) {
    stop(sprintf(
      "data for parameterCode '%s' has not been processed",
      parameterCode
    ))
  }

  year <- as.numeric(year)

  lastYear <- lubridate::now(tzone = "UTC") %>% lubridate::year() - 1

  if ( parameterCode == "42101" ) {
    parameter <- "CO"
    if ( !year %in% 1980:lastYear) {
      stop(sprintf(
        "No EPA data available for parameter code %s in year %i",
        parameterCode, year)
      )
    }
  } else if ( parameterCode == "44201" ) {
    parameter <- "OZONE"
    if ( !year %in% 1980:lastYear) {
      stop(sprintf(
        "No EPA data available for parameter code %s in year %i",
        parameterCode, year)
      )
    }
  } else if ( parameterCode == "88101" ) {
    parameter <- "PM2.5"
    if ( !year %in% 2008:lastYear) {
      stop(sprintf(
        "No EPA data available for parameter code %s in year %i",
        parameterCode, year)
      )
    }
  } else if  ( parameterCode == "88502" ) {
    parameter <- "PM2.5"
    if ( !year %in% 1998:lastYear) {
      stop(sprintf(
        "No EPA data available for parameter code %s in year %i",
        parameterCode, year)
      )
    }
  }

  # ----- Load data ------------------------------------------------------------

  # Create file name and path according to the AirMonitorIngest scheme

  if ( is.null(baseUrl) ) {
    dataUrl <- NULL
  } else {
    dataUrl <- file.path(baseUrl, "epa_aqs", parameterCode, year)
  }

  if ( is.null(baseDir) ) {
    dataDir <- NULL
  } else {
    dataDir <- file.path(baseDir, "epa_aqs", parameterCode, year)
  }

  metaFileName <- sprintf("epa_aqs_%s_%s_meta.rda", parameterCode, year)
  dataFileName <- sprintf("epa_aqs_%s_%s_data.rda", parameterCode, year)

  meta <- MazamaCoreUtils::loadDataFile(metaFileName, dataUrl, dataDir)
  data <- MazamaCoreUtils::loadDataFile(dataFileName, dataUrl, dataDir)

  monitor <- list(meta = meta, data = data)

  monitor <- structure(monitor, class = c("mts_monitor", "mts", class(monitor)))

  # ----- Return ---------------------------------------------------------------

  MazamaTimeSeries::mts_check(monitor)

  return(monitor)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  year <- 2015
  parameterCode <- 88101
  baseUrl <- NULL
  baseDir <- "~/Data/monitoring"



  monitor <- epa_loadAnnual(
    year = year,
    parameterCode = parameterCode,
    baseUrl = baseUrl,
    baseDir = baseDir
  )

  example_88101 <-
    monitor <- epa_aqs_loadAnnual(
      year = 2015,
      parameterCode = 88101,
      baseUrl = NULL,
      baseDir = "~/Data/monitoring"
    ) %>%
    monitor_filterMeta(stateCode %in% c("WA", "OR", "ID")) %>%
    monitor_filterDate(20150601, 20151101)

}
