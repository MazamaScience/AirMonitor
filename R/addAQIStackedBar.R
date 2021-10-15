#' @export
#' @title Create Stacked AQI Bar
#' @param pollutant EPA AQS criteria pollutant.
#' @param width Width of the bar as a fraction of the width of the plot area.
#' @param height Height of the bar as a fraction of the height of the plot area.
#' @param pos Position of the stacked bar relative to the plot.
#' @description Draws a stacked bar indicating AQI levels on one side of a plot
#' @return Stacked AQI Bar

addAQIStackedBar <- function(
  pollutant = c("PM2.5", "CO"),
  width = .01,
  height = 1,
  pos = c("left", "right")
) {

  pollutant <- match.arg(pollutant)
  pos <- match.arg(pos)

  usr <- par("usr")

  if (pos == "right") {
    l <- usr[2] - width*(usr[2] - usr[1])
    r <- usr[2]
  } else if (pos == "left") {
    l <- usr[1]
    r <- usr[1] + width*(usr[2] - usr[1])
  }

  # for (i in 1:6) {
  #   rect(
  #     xleft = l,
  #     ybottom = min(max(0,AQI$breaks_24[i]), height*usr[4]),
  #     xright = r,
  #     ytop = min(AQI$breaks_24[i+1], height*usr[4]),
  #     col = AQI$colors[i],
  #     xpd = NA,
  #     border = NA
  #   )
  # }

  warning("Not functioning yet.")

}
