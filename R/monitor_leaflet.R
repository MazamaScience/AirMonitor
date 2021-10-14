#' @export
#'
#' @title Leaflet interactive map of monitor locations
#'
#' @param monitor \emph{mts_monitor} object.
#' @param slice Either a formatted time string, a time index, the name of a
#' (potentially user defined) function used to collapse the time axis.
# @param breaks set of breaks used to assign colors
# @param colors a set of colors for different levels of air quality data
#   determined by \code{breaks}
# @param labels a set of text labels, one for each color
# @param legendTitle legend title
#' @param radius radius of monitor circles
#' @param opacity opacity of monitor circles
#' @param maptype optional name of leaflet ProviderTiles to use, e.g. "terrain"
# @param popupInfo a vector of column names from monitor$meta to be shown in
#   a popup window
#' @param extraVars Character vector of additional column names from \code{monitor$meta}
#' to be shown in leaflet popups.
#' @param jitter Amount to use to slightly adjust locations so that multiple
#' monitors at the same location can be seen. Use zero or \code{NA} to see
#' precise locations.
#' @param ... Additional arguments passed to \code{leaflet::addCircleMarker()}.
#'
#' @description
#' This function creates interactive maps that will be displayed in RStudio's
#' 'Viewer' tab. The \code{slice} argument is used to collapse a
#' \emph{mts_monitor} timeseries into a single value. If \code{slice} is an
#' integer, that row index will be selected from the \code{monitor$data}
#' dataframe. If \code{slice} is a function (unquoted), that function will be
#' applied to the timeseries with the argument \code{na.rm=TRUE} (e.g.
#' \code{max(..., na.rm=TRUE)}).
#'
#' If \code{slice} is a user defined function, it will be used with argument
#' \code{na.rm=TRUE} to collapse the time dimension. Thus, user defined
#' functions must accept \code{na.rm} as an argument.
#'
#' @details
#' The \code{maptype} argument is mapped onto leaflet "ProviderTile" names.
#' Current map types include:
#'
#' \enumerate{
#' \item{"roadmap"}{ -- "OpenStreetMap"}
#' \item{"satellite"}{ -- "Esri.WorldImagery"}
#' \item{"terrain"}{ -- "Esri.WorldTopoMap"}
#' \item{"toner"}{ -- "Stamen.Toner"}
#' }
#'
#' If a character string not listed above is provided, it will be used as the
#' underlying map tile if available. See
#' \url{https://leaflet-extras.github.io/leaflet-providers/} for a list of
#' "provider tiles" to use as the background map.
#'
#' @return Invisibly returns a leaflet map of class "leaflet".
#'

monitor_leaflet <- function(
  monitor,
  slice = "max",
  radius = 10,
  opacity = 0.7,
  maptype = "terrain",
  extraVars = NULL,
  jitter = 5e-4,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  monitor_check(monitor)

  if ( monitor_isEmpty(monitor) )
    stop("monitor object has no data")

  MazamaCoreUtils::setIfNull(slice, "max")
  MazamaCoreUtils::setIfNull(maptype, "terrain")

  if ( !is.null(extraVars) ) {
    unrecognizedVars <- setdiff(extraVars, names(monitor$meta))
    if ( length(unrecognizedVars) > 0 ) {
      stop("variables in 'extraVars' not found in 'monitor$meta'")
    }
  }

  if ( is.null(jitter) || is.na(jitter) ) {
    jitter <- 0
  }

  # ----- Pollutant dependent AQI ----------------------------------------------

  # See: https://aqs.epa.gov/aqsweb/documents/codetables/aqi_breakpoints.html

  pollutant <- toupper(unique(monitor$meta$pollutant))
  if ( length(pollutant) > 1 ) {
    pollutantString <- paste0(pollutant, collapse = ", ")
    stop(sprintf("multiple pollutants found: %s", pollutantString))
  }

  if ( pollutant == "CO" ) {
    AQI_breaks_24 <- c(-Inf, 4.5, 9.5, 12.5, 15.5, 30.5, Inf)
    AQI_colors <- c("#00E400", "#FFFF00", "#FF7E00", "#FF0000", "#8F3F97", "#7E0023")
    AQI_names <- c("Good", "Moderate", "USG", "Unhealthy", "Very Unhealthy", "Hazardous")
    legendTitle <- "CO AQI Level"
    units <- "ppm"
    digits <- 1
  } else if ( pollutant == "PM2.5" ) {
    AQI_breaks_24 <- c(-Inf, 12.0, 35.5, 55.5, 150.5, 250.5, Inf)
    AQI_colors <- c("#00E400", "#FFFF00", "#FF7E00", "#FF0000", "#8F3F97", "#7E0023")
    AQI_names <- c("Good", "Moderate", "USG", "Unhealthy", "Very Unhealthy", "Hazardous")
    legendTitle <- "PM2.5 AQI Level"
    units <- "\U00B5g/m3"
    digits <- 0
  }

  breaks <- AQI_breaks_24
  colors <- AQI_colors
  labels <- AQI_names

  # ----- Create the 'slice' values --------------------------------------------

  if ( is.numeric(slice) ) {

    # * slice = number -----

    if ( slice < 1 || slice > nrow(monitor$data) )
      stop(sprintf("slice = %d is outside the range 1:%d", slice, nrow(monitor$data)))

    slice <- round(slice)

    popupValue <- as.numeric(dplyr::slice(monitor$data, slice)[-1])

    popupWhen <-
      strftime(monitor$data$datetime[slice], "on %B %d, %Y at %H:00", tz = "UTC", usetz = TRUE)

  } else if ( is.character(slice) ) {

    # * slice = character -----

    if ( exists(slice) && (class(get(slice)) == "function") ) {

      # ** slice = function -----

      FUN <- get(slice)

      # NOTE:  min/max will warn and return Inf/-Inf when all data are missing
      # NOTE:  while mean returns NaN so we need to suppress warnings and replace
      # NOTE: all those non-finite values with NA.

      suppressWarnings({
        popupValue <-
          apply(monitor$data[,-1], 2, FUN, na.rm = TRUE, simplify = TRUE)
      })

      popupValue[!is.finite(popupValue)] <- NA

      popupWhen <- ""

      legendTitle <- sprintf("%s %s", stringr::str_to_title(slice), legendTitle)

      if ( slice == "max" ) {

        # Can't find a good dplyr way to get the first occurrance of each value so we roll our own

        suppressWarnings({

          dataBrick <- monitor$data[, -1]
          sliceValueBrick <- matrix(rep(popupValue, nrow(dataBrick)), nrow = nrow(dataBrick), byrow = TRUE)
          logicalBrick <- dataBrick == sliceValueBrick
          logicalBrick[is.na(logicalBrick)] <- FALSE
          firstRowAtMax <- apply(logicalBrick, 2, function(x) { min(which(x), na.rm = TRUE) },  simplify = TRUE)
          firstRowAtMax[!is.finite(firstRowAtMax)] <- NA
          firstTimeAtMax <- monitor$data$datetime[firstRowAtMax]

        })

        popupWhen <-
          strftime(firstTimeAtMax, "on %B %d at %H:00", tz = "UTC", usetz = TRUE)

        # TODO:  Idea for local time not working yet.

        # popupWhen <- vector("character", length(firstTimeAtMax))
        # for ( i in seq_len(firstTimeAtMax) ) {
        #   popupWhen[i] <-
        #     strftime(firstTimeAtMax[i], "on %B %d, %Y at %H:00", tz = monitor$meta$timezone[i], usetz = TRUE)
        # }

        popupWhen[is.na(popupWhen)] <- ""

      } # END of slice == "max"

    } else {

      # ** slice = datetime -----

      result <- try({
        sliceTime <- MazamaCoreUtils::parseDatetime(slice, timezone = "UTC")
      }, silent = TRUE)

      if ( "try-error" %in% class(result) ) {

        stop("improper use of slice parameter")

      } else {

        # Now proceed as if slice were an integer

        slice <- which(monitor$data$datetime == sliceTime)

        popupValue <- as.numeric(dplyr::slice(monitor$data, slice)[-1])

        popupWhen <-
          strftime(monitor$data$datetime[slice], "on %B %d, %Y at %H:00", tz = "UTC", usetz = TRUE)

      }

    } # END slice = character

  } else {

    # * slice = neither -----

    stop("improper use of slice parameter")

  }

  # ----- Create colors and legend labels --------------------------------------

  # If the user didn't use custom breaks then use AQI names and colors
  if ( all.equal(breaks, AQI_breaks_24) && all.equal(colors, AQI_colors) ) {

    # Ignore warnings from RColorBrewer as leaflet::colorBin does the right thing
    suppressWarnings({
      colorFunc <- leaflet::colorBin(AQI_colors,
                                     bins = AQI_breaks_24,
                                     na.color = "#bbbbbb")
      cols <- colorFunc(popupValue)
      colors <- AQI_colors
      labels <- AQI_names
    })

  } else {

    if ( length(breaks) <= 2) {
      stop("Please specify the correct vector of breaks")
    }

    if (! (length(breaks) - 1 == length(colors)) ) {
      stop("The number of colors provided should be one less than the number of breaks")
    }

    if ( missing(labels) ){
      labels <- paste(sprintf("%.1f", breaks[-length(breaks)]),
                      "--",
                      sprintf("%.1f", breaks[-1]))
    } else if ( length(labels) != length(colors) ) {
      stop("The number of labels should be equal to the number of colors")
    }

    # Create levels and use them to create a color mask
    levels <- .bincode(popupValue, breaks, include.lowest = TRUE)
    if ( !all(!is.na(levels)) ) {
      print("NOTE that there are data points outside of your specified breaks, non-requested color(s) might be displayed on your map.")
    }
    cols <- colors[levels]

  }

  # ----- Create popup text ----------------------------------------------------

  popupText <- paste0(
    "<b>", monitor$meta$locationName, "</b><br>",
    "<b>", round(popupValue, digits), " ", units, "</b> ", popupWhen, "<br><br>",
    monitor$meta$deviceDeploymentID, "<br>",
    "locationID = ", monitor$meta$locationID, "<br>",
    "deviceID = ", monitor$meta$deviceID, "<br><br>",
    monitor$meta$county, " County, ", monitor$meta$stateCode, "<br>",
    "timezone = ", monitor$meta$timezone, "<br>",
    "longitude = ", monitor$meta$longitude, ", ", "latitude = ", monitor$meta$latitude, "<br>"
  )

  # Add extra vars
  for ( i in seq_along(popupText) ) {

    extraText <- vector("character", length(extraVars))
    for ( j in seq_along(extraVars) ) {
      var <- extraVars[j]
      extraText[j] <- paste0(var, " = ", monitor$meta[i, var], "<br>")
    }
    extraText <- paste0(extraText, collapse = "")

    popupText[i] <- paste0(popupText[i], "<hr>", extraText)
  }

  monitor$meta$popupText <- popupText

  # ----- Create map -----------------------------------------------------------

  # Determine appropriate zoom level
  lonRange <- range(monitor$meta$longitude, na.rm = TRUE)
  latRange <- range(monitor$meta$latitude, na.rm = TRUE)
  maxRange <- max(diff(lonRange), diff(latRange), na.rm = TRUE)

  if (maxRange > 20) {
    zoom <- 4
  } else if (maxRange > 10) {
    zoom <- 5
  } else if (maxRange > 5) {
    zoom <- 6
  } else if (maxRange > 2) {
    zoom <- 7
  } else if (maxRange > 1) {
    zoom <- 8
  } else if (maxRange > 0.5) {
    zoom <- 9
  } else if (maxRange > 0.2) {
    zoom <- 10
  } else if (maxRange > 0.1) {
    zoom <- 11
  } else {
    zoom <- 12
  }

  # ----- Create leaflet map ---------------------------------------------------

  # Filter out missing location data
  monitor$meta <-
    monitor$meta %>%
    dplyr::filter(!is.na(.data$longitude)) %>%
    dplyr::filter(!is.na(.data$latitude))

  # Spread out locations if requested
  if ( jitter > 0 ) {
    monitor$meta <-
      monitor$meta %>%
      dplyr::mutate(
        longitude = jitter(.data$longitude, amount = 5e-4),
        latitude = jitter(.data$latitude, amount = 5e-4)
      )
  }

  # Convert maptype to a character string that addProviderTiles can read
  if ( missing(maptype) || maptype == "terrain") {
    providerTiles <- "Esri.WorldTopoMap"
  } else if ( maptype == "roadmap" ) {
    providerTiles <- "OpenStreetMap"
  } else if ( maptype == "toner" ) {
    providerTiles <- "Stamen.Toner"
  } else if (maptype == "satellite" ) {
    providerTiles <- "Esri.WorldImagery"
  } else {
    providerTiles <- maptype
  }

  # Create leaflet map
  leafletMap <-
    leaflet::leaflet(monitor$meta) %>%
    leaflet::setView(lng = mean(lonRange), lat = mean(latRange), zoom = zoom) %>%
    leaflet::addProviderTiles(providerTiles) %>%
    leaflet::addCircleMarkers(
      lat = ~latitude,
      lng = ~longitude,
      radius = radius,
      fillColor = cols,
      fillOpacity = opacity,
      stroke = FALSE,
      popup = monitor$meta$popupText,
      ...) %>%
    leaflet::addLegend(
      position = "bottomright",
      colors = rev(colors), # show low levels at the bottom
      labels = rev(labels),  # show low levels at the bottom
      opacity = 1,
      title = legendTitle
    )

  return(leafletMap)

}
