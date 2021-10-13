#'
#' @docType package
#' @name AirMonitor
#' @title Air Quality Data Analysis
#' @description Tools for working with air quality data.

NULL

# ----- Internal Data -------------------------------------------------

#' coreMetadataNames
#'
#' @export
#' @docType data
#' @name coreMetadataNames
#' @title Names of standard metadata columns
#' @format A vector of character strings
#' @description Vector of names of the required \code{monitor$meta} columns.
#' These represent metadata columns that must exist in every valid
#' \emph{mts_monitor} object. Any number of additional columns may also be present.

coreMetadataNames <- c(
  "deviceDeploymentID",       # -- timeseries unique identifier
  "deviceID",                 # -- device unique identifier
  "deviceType",               # -- non-standardized identifier for the type of device (e.g. EBAM, ESAM, PA, ...)
  "deviceDescription",        # -- human readable details
  "deviceExtra",              # -- extra device information (e.g. as JSON)
  "pollutant",                # -- one of pollutantNames below
  "units",                    # -- one of ???# TODO: sort out units standardization
  "dataIngestSource",         # -- non-standardized identifier for the data source (e.g. AIRNOW, WRCC, ...)
  "dataIngestURL",            # -- top level URL
  "dataIngestUnitID",         # -- unique identifier used to extract the data
  "dataIngestExtra",          # -- extra data ingest information (e.g. as JSON)
  "dataIngestDescription",    # -- human readable details
  "locationID",               # -- from MazamaLocationUtils
  "locationName",             # -- from MazamaLocationUtils
  "longitude",                # -- from MazamaLocationUtils
  "latitude",                 # -- from MazamaLocationUtils
  "elevation",                # -- from MazamaLocationUtils
  "countryCode",              # -- from MazamaLocationUtils
  "stateCode",                # -- from MazamaLocationUtils
  "county",                   # -- from MazamaLocationUtils
  "timezone",                 # -- from MazamaLocationUtils
  "houseNumber",              # -- from MazamaLocationUtils
  "street",                   # -- from MazamaLocationUtils
  "city",                     # -- from MazamaLocationUtils
  "zip"                       # -- from MazamaLocationUtils
)


#' pollutantNames
#'
#' @export
#' @docType data
#' @name pollutantNames
#' @title Names of standard pollutants
#' @format A vector of character strings
#' @description Character string identifiers of recognized pollutants.

pollutantNames <- c(
  "OZONE",
  "SO2",
  "CO",
  "NO",
  "PM2.5",
  "PM10"
)

