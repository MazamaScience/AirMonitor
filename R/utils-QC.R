#' @export
#'
#' @title Invalidate consecutive suspect values.
#'
#' @param x Timeseries data.
#' @param suspectValues Vector of numeric values considered suspect.
#' @param consecutiveCount How many \code{suspectValues} must appear in a row
#' before they are invalidated.
#'
#' @description Invalidates values within a timeseries that appear "sticky".
#' Some temporary monitoring data has stretches of consecutive values, sometimes
#' well outside the range of reasonable. This QC function identifies these
#' "sticky" stretches and returns the original timeseries data with "sticky"
#' stretches replaced with \code{NA}.
#'
#' @return Returns \code{x} with some values potentially replaced with \code{NA}.

QC_invalidateConsecutiveSuspectValues <- function(
    x = NULL,
    suspectValues = c(0:10 * 1000, NA),
    consecutiveCount = 2
) {

  # Create a mask of suspect values using 1/0 instead of T/F
  isSuspect <- as.numeric(x %in% suspectValues)

  # Left aligned consecutive count
  left <-
    MazamaRollUtils::roll_sum(
      isSuspect,
      width = 2,
      by = 1,
      align = "left",
      na.rm = FALSE
    )

  # Right aligned consecutive_count
  right <-
    MazamaRollUtils::roll_sum(
      isSuspect,
      width = 2,
      by = 1,
      align = "right",
      na.rm = FALSE
    )

  # NOTE:  Mask is TRUE only when a value is part of a sequence of N or more
  # NOTE:  consecutive suspect values.
  mask <- (left >= consecutiveCount) | (right >= consecutiveCount)

  # Invalidate these values
  x[mask] <- as.numeric(NA)

  return(x)

}
