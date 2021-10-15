#'
#' #' @export
#' #' @title Add an AQI Legend to a Map
#' #' @param x x coordinate passed on to the \code{legend()} command.
#' #' @param y y coordinate passed on to the \code{legend()} command.
#' #' @param col Color for points/lines in the legend.
#' #' @param legend Character vector to be shown in the legend.
#' #' @param pch Plotting symbols in the legend.
#' #' @param title Title for the legend.
#' #' @param ... Additional arguments to be passed to \code{legend()}.
#' #' @description This function is a convenience wrapper around
#' #' \code{graphics::legend()}. It will show the AQI colors and
#' #' names by default if \code{col} and \code{legend} are not specified.
#'
#' addAQILegend <- function(
#'   x = "topright",
#'   y = NULL,
#'   col = rev(AQI$colors),
#'   legend = rev(AQI$names),
#'   pch = 16,
#'   title = "Air Quality Index",
#'   ...
#' ) {
#'
#'   legend(
#'     x = x,
#'     y = y,
#'     col = col,
#'     legend = legend,
#'     pch = pch,
#'     title = title,
#'     ...
#'   )
#'
#' }
