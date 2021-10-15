#' @export
#' @importFrom grDevices adjustcolor
#' @importFrom graphics rect par
#' @title Add nighttime shading to a timeseries plot
#' @param timeInfo dataframe as returned by \code{monitor_timeInfo()}
#' @param col Color used to shade nights.
#' @description Draw shading rectangles on a plot to indicate nighttime hours.
#' @seealso \link{timeInfo}

addShadedNight <- function(
  timeInfo,
  col = adjustcolor('black', 0.1)
) {

  # ----- Validate parameters --------------------------------------------------

  localTime <- timeInfo$localTime
  sunrise <- timeInfo$sunrise[!duplicated(timeInfo$sunrise)]
  sunset <- timeInfo$sunset[!duplicated(timeInfo$sunset)]

  # Sanity check
  if ( any(sunset < sunrise) )
    stop("sunset before sunrise???")

  # ----- Shaded nights --------------------------------------------------------

  # Left edge to first sunrise
  if ( localTime[1] < sunrise[1] ) {
    rect(
      xleft = par('usr')[1],
      ybottom = par('usr')[3],
      xright = sunrise[1],
      ytop = par('usr')[4],
      col = col,
      border = NA
    )
  }

  # Complete nights
  if ( length(sunset) > 1 ) {
    for ( i in seq(length(sunset) - 1) ) {
      rect(
        xleft = sunset[i],
        ybottom = par('usr')[3],
        xright = sunrise[i + 1],
        ytop = par('usr')[4],
        col = col,
        border = NA
      )
    }
  }

  # Last sunset to right edge
  rect(
    xleft = sunset[length(sunset)],
    ybottom = par('usr')[3],
    xright = par('usr')[2],
    ytop = par('usr')[4],
    col = col,
    border = NA
  )

}

