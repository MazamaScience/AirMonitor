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
  # Specific to AirMonitor
  "deviceDeploymentID",       # -- timeseries unique identifier
  "deviceID",                 # -- device unique identifier
  "deviceType",               # -- internally-standardized identifier for the type of device (e.g. EBAM, ESAM, PA, ...)
  "deviceDescription",        # -- human readable device details
  "deviceExtra",              # -- extra device information (possibly as JSON)
  "pollutant",                # -- one of "OZONE|CO|NO2|PM2.5|PM10"
  "units",                    # -- one of "PPM|PPB|MILLIGRAMS|MICROGRAMS"
  "dataIngestSource",         # -- internally-standardized identifier for the data source (e.g. AIRNOW, WRCC, ...)
  "dataIngestURL",            # -- top level URL
  "dataIngestUnitID",         # -- unique identifier used to extract data from the URL
  "dataIngestExtra",          # -- extra data ingest information (possibly as JSON)
  "dataIngestDescription",    # -- human readable data ingest details

  # Defined in MazamaLocationUtils
  "locationID",               # -- location unique identifier
  "locationName",             # -- human readable location name
  "longitude",                # --
  "latitude",                 # --
  "elevation",                # --
  "countryCode",              # -- ISO 3166-1 alpha-2
  "stateCode",                # -- ISO 3166-2 alpha-2
  "county",                   # --
  "timezone",                 # -- Olson time zone
  "houseNumber",              # --
  "street",                   # --
  "city",                     # --
  "zip"                       # --
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

