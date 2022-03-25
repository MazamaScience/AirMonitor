#' @export
#' @importFrom rlang .data
#' @importFrom dplyr across everything na_if
#'
#' @title Convert monitor data as CSV
#'
#' @param monitor \emph{mts_monitor} object.
#' @param includeMeta Logical specifying whether to include \code{monitor$meta}.
#' @param includeData Logical specifying whether to include \code{monitor$data}.
#'
#' @description Converts the contents of the \code{monitor} argument to CSV.
#' By default, the output is a text string with "human readable" CSV that
#' includes both \code{meta} and \code{data}. When saved as a file, this format
#' is useful for point-and-click spreadsheet users who want to have everything
#' on a single sheet.
#'
#' To obtain a machine parseable CSV string for just the data, you can use
#' \code{includeMeta = FALSE}. To obtain machine parseable metadata, use
#' \code{includeData = FALSE}.
#'
#' @return CSV formatted text.
#'
#' @examples
#' library(AirMonitor)
#'
#' monitor <-
#'   Carmel_Valley %>%
#'   monitor_filterDate(20160802, 20160803)
#'
#' monitor_toCSV(monitor) %>% cat()
#' monitor_toCSV(monitor, includeData = FALSE) %>% cat()
#' monitor_toCSV(monitor, includeMeta = FALSE) %>% cat()
#'

monitor_toCSV <- function(
  monitor,
  includeMeta = TRUE,
  includeData = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  includeMeta <- MazamaCoreUtils::setIfNull(includeMeta, TRUE)
  includeData <- MazamaCoreUtils::setIfNull(includeData, TRUE)

  if ( !monitor_isValid(monitor) )
    stop("Parameter 'monitor' is not a valid 'mts_monitor' object.")

  if ( monitor_isEmpty(monitor) )
    stop("Parameter 'monitor' has no data.")

  # ----- Early return if possible ---------------------------------------------

  if ( !includeMeta && !includeData ) {

    return("")

  } else if ( includeMeta && !includeData ) {

    # Nothing fancy, just the monitor$meta dataframe
    csvText <- readr::format_csv(monitor$meta, na = "NA", col_names = TRUE)
    return(csvText)

  }

  # ----- Create metaTbl -------------------------------------------------------

  # Structure the metaMatrix so that columns match up with dataTbl columns
  # NOTE:  second column gets NA to align with the data 'Local Time' column

  metaMatrix <-
    cbind(
      AirMonitor::coreMetadataNames,
      "",
      t(monitor$meta[,AirMonitor::coreMetadataNames])
    )

  # To avoid dplyr .name_repair issues
  colnames(metaMatrix) <- c("parameter", "blank", monitor$meta$deviceDeploymentID)

  metaTbl <- dplyr::as_tibble(metaMatrix, .name_repair = "check_unique")

  # ---- Create dataTbl --------------------------------------------------------

  # localTime determination
  timezones <- monitor$meta$timezone
  timezone <- ifelse( length(unique(timezones)) == 1, unique(timezones), "UTC" )

  # datetime from a monitor object should always be UTC
  utcTime <- lubridate::with_tz(monitor$data$datetime, tzone = "UTC")

  # Save character string representations of utcTime and localTime
  # localTime defaults to UTC if > 1 timezone involved
  utcTimeString <- strftime(utcTime, "%Y-%m-%d %H:%M:%S %Z", tz = "UTC")
  localTimeString <- strftime(utcTime, "%Y-%m-%d %H:%M:%S %Z", tz = timezone)

  dataMatrix <-
    cbind(
      utcTimeString,
      localTimeString,
      monitor$data[,-1]
    )

  # To avoid dplyr .name_repair issues
  colnames(dataMatrix) <- make.names(1:ncol(dataMatrix))

  dataTbl <-
    dplyr::as_tibble(dataMatrix, .name_repair = "check_unique") %>%
    # Convert "NaN" to NA
    dplyr::mutate(across(everything(), ~ na_if(., "NaN")))

  if ( length(unique(timezones)) == 1 ) {
    names(dataTbl) <- c("UTC Time", "Local Time", monitor$meta$deviceDeploymentID)
  } else {
    names(dataTbl) <- c("UTC Time", "UTC Time (no Local Time because > 1 monitor timezone)", monitor$meta$deviceDeploymentID)
  }

  # ---- Assemble desired output -----------------------------------------------

  # Two possible output formats remain

  if ( !includeMeta && includeData ) {

    # Use the improved 'data' dataframe
    csvText <- readr::format_csv(dataTbl, na = "NA", col_names = TRUE)

  } else {

    # Fancy, "human readable" format appropriate for point-and-click Excel users

    # Create fake tibble to use as human-readable separators
    emptyRow <- metaTbl %>% dplyr::filter(.data$parameter == "DONT MATCH ME")
    metaSeparator <- emptyRow
    metaSeparator[1,1] <- "##### Site metadata begins below here"
    dataSeparator <- emptyRow
    dataSeparator[1,1] <- "##### Hourly data begins below here"

    # Format as CSV and combine into a "fake file" text string
    emptyRowText <- readr::format_csv(emptyRow, na = "NA", col_names = FALSE)
    metaHeaderText <- readr::format_csv(metaSeparator, na = "NA", col_names = FALSE)
    metaBodyText <- readr::format_csv(metaTbl, na = "NA", col_names = TRUE)
    dataHeaderText <- readr::format_csv(dataSeparator, na = "NA", col_names = FALSE)
    dataBodyText <- readr::format_csv(dataTbl, na = "NA", col_names = TRUE)

    csvText <- paste0(
      metaHeaderText,
      metaBodyText,
      emptyRowText,
      dataHeaderText,
      dataBodyText,
      collapse = "\n"
    )

  }

  # ---- Return ----------------------------------------------------------------

  return(csvText)

}
