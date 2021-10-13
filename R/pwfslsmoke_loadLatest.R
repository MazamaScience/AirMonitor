#' #' @export
#' #'
#' #' @title Load latest monitoring data from the PWFSLSmoke archive
#' #'
#' #' @param baseUrl Base URL for PWFSLSmoke latest data files in case it changes.
#' #'
#' #' @return A \emph{mts_monitor} object with PWFSLSmoke latest data.
#' #'
#' #' @description Loads pre-generated latest PM2.5 data used in the \pkg{PWFSLSmoke}
#' #' package.
#' #'
#' #'
#'
#' pwfslsmoke_loadLatest <- function(
#'   baseUrl = "https://haze.airfire.org/monitoring/latest/RData"
#' ) {
#'
#'   # ----- Validate parameters --------------------------------------------------
#'
#'   MazamaCoreUtils::setIfNull(baseUrl, "https://haze.airfire.org/monitoring/latest/RData")
#'
#'   # ----- AirNow data ----------------------------------------------------------
#'
#'   fileName <- paste0("airnow_PM2.5_latest10.RData")
#'   ws_monitor <- MazamaCoreUtils::loadDataFile(fileName, baseUrl, dataDir = NULL)
#'
#'   # * meta -----
#'
#'   # NOTE:  Here are the commonalities:
#'   # NOTE:    > intersect(names(ws_monitor$meta), names(example_88101$meta))
#'   # NOTE:    [1] "longitude"   "latitude"    "elevation"   "timezone"    "countryCode" "stateCode"
#'
#'   commonColumns <- intersect(names(ws_monitor$meta), names(example_88101$meta))
#'   requiredColumns <- names(example_88101$meta)[1:15]
#'
#'   # > setdiff(requiredColumns, commonColumns)
#'   # [1] "deviceDeploymentID" "deviceID"           "locationID"         "locationName"
#'   # [5] "county"             "houseNumber"        "street"             "city"
#'   # [9] "zip"
#'
#' # TODO:  Put this in a separate monitor_fromPWFSLSmoke() function
#'
#'   meta <-
#'     ws_monitor$meta %>%
#'
#'     # Add locationID
#'     dplyr::mutate(
#'       locationID = MazamaLocationUtils::location_createID(.data$longitude, .data$latitude),
#'       deviceID = .data$monitorID
#'     ) %>%
#'
#'     dplyr::mutate(
#'       deviceDeploymentID = paste0(.data$locationID, "_", .data$deviceID),
#'       locationName = .data$siteName,
#'       county = .data$countyName,
#'       houseNumber = as.character(NA),
#'       street = as.character(NA),
#'       city = as.character(NA),
#'       zip = as.character(NA)
#'     )
#'
#'   pwfslColumns <- setdiff(names(meta), requiredColumns)
#'   newColumns <- c(requiredColumns, pwfslColumns)
#'
#'   meta <-
#'     meta %>% dplyr::select(all_of(newColumns))
#'
#'
#'
#'   if ( is.null(baseUrl) && is.null(baseDir) )
#'     stop("one of 'baseUrl' or 'baseDir' must be defined")
#'
#'   # Parameter code
#'   validParameterCodes <- c(
#'     # "44201",
#'     # "42401",
#'     # "42101",
#'     # "42602",
#'     "88101",
#'     "88502"
#'     # "81102",
#'     # "SPEC",
#'     # "WIND",
#'     # "TEMP",
#'     # "PRESS",
#'     # "RH_DP",
#'     # "HAPS",
#'     # "VOCS",
#'     # "NONOxNOy"
#'   )
#'
#'   parameterCode <- as.character(parameterCode)
#'   if ( !parameterCode %in% validParameterCodes ) {
#'     stop(sprintf(
#'       "data for parameterCode '%s' has not been processed",
#'       parameterCode
#'     ))
#'   }
#'
#'   year <- as.numeric(year)
#'
#'   lastYear <- lubridate::now(tzone = "UTC") %>% lubridate::year() - 1
#'
#'   if ( parameterCode == "88101" ) {
#'     parameter <- "PM2.5"
#'     if ( !year %in% 2008:lastYear) {
#'       stop(sprintf(
#'         "No EPA data available for parameter code %s in year %i",
#'         parameterCode, year)
#'       )
#'     }
#'   } else if  ( parameterCode == "88502" ) {
#'     parameter <- "PM2.5"
#'     if ( !year %in% 1998:lastYear) {
#'       stop(sprintf(
#'         "No EPA data available for parameter code %s in year %i",
#'         parameterCode, year)
#'       )
#'     }
#'   }
#'
#'   # ----- Load data ------------------------------------------------------------
#'
#'   # Create file name and path according to the AirMonitorIngest scheme
#'
#'   if ( is.null(baseUrl) ) {
#'     dataUrl <- NULL
#'   } else {
#'     dataUrl <- file.path(baseUrl, "epa_aqs", parameterCode, year)
#'   }
#'
#'   if ( is.null(baseDir) ) {
#'     dataDir <- NULL
#'   } else {
#'     dataDir <- file.path(baseDir, "epa_aqs", parameterCode, year)
#'   }
#'
#'   metaFileName <- sprintf("epa_aqs_%s_%s_meta.rda", parameterCode, year)
#'   dataFileName <- sprintf("epa_aqs_%s_%s_data.rda", parameterCode, year)
#'
#'   meta <- MazamaCoreUtils::loadDataFile(metaFileName, dataUrl, dataDir)
#'   data <- MazamaCoreUtils::loadDataFile(dataFileName, dataUrl, dataDir)
#'
#'   monitor <- list(meta = meta, data = data)
#'
#'   monitor <- structure(monitor, class = c("mts_monitor", "mts", class(monitor)))
#'
#'   # ----- Return ---------------------------------------------------------------
#'
#'   MazamaTimeSeries::mts_check(monitor)
#'
#'   return(monitor)
#'
#' }
#'
#' # ===== DEBUGGING ==============================================================
#'
#' if ( FALSE ) {
#'
#'   year <- 2015
#'   parameterCode <- 88101
#'   baseUrl <- NULL
#'   baseDir <- "~/Data/monitoring"
#'
#'
#'
#'   monitor <- epa_loadAnnual(
#'     year = year,
#'     parameterCode = parameterCode,
#'     baseUrl = baseUrl,
#'     baseDir = baseDir
#'   )
#'
#'   example_88101 <-
#'     monitor <- epa_loadAnnual(
#'       year = 2015,
#'       parameterCode = 88101,
#'       baseUrl = NULL,
#'       baseDir = "~/Data/monitoring"
#'     ) %>%
#'     monitor_filterMeta(stateCode %in% c("WA", "OR", "ID")) %>%
#'     monitor_filterDate(20150601, 20151101)
#'
#' }
