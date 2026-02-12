#' @export
#'
#' @title Convert PM2.5 NowCast concentration (µg/m³) to AQI
#'
#' @param x data.frame / tibble of numeric columns (no datetime).
#' @param NAAQS Version of NAAQS levels to use. See Note.
#'
#' @return data.frame of integer AQI values (same dimensions).
#'
#' @description
#' Input must be a data.frame/tibble of numeric columns (no datetime column).
#' Output is a same-shaped data.frame of integer AQI values.
#'
#' Behavior:
#'   - NA / non-finite -> NA
#'   - x <= 0 -> 0
#'   - truncate PM2.5 to 0.1 µg/m³ (floor, not round)
#'   - extended AQI using last segment for values above top breakpoint (no cap)
#'
#' @note
#' On February 7, 2024, EPA strengthened the National Ambient Air Quality
#' Standards for Particulate Matter (PM NAAQS) to protect millions of Americans
#' from harmful and costly health impacts, such as heart attacks and premature
#' death. Particle or soot pollution is one of the most dangerous forms of air
#' pollution, and an extensive body of science links it to a range of serious
#' and sometimes deadly illnesses. EPA is setting the level of the primary
#' (health-based) annual PM2.5 standard at 9.0 micrograms per cubic meter to
#' provide increased public health protection, consistent with the available
#' health science.
#' See \href{https://www.epa.gov/pm-pollution/final-reconsideration-national-ambient-air-quality-standards-particulate-matter-pm}{PM NAAQS update}.
#'
#' @references \url{https://en.wikipedia.org/wiki/Nowcast_(Air_Quality_Index)}
#' @references \url{https://www.airnow.gov/aqi/aqi-basics/}
#'

nowcast_to_aqi <- function(
    x,
    NAAQS = c("PM2.5_2024", "PM2.5")
) {

  parameterName <- "PM2.5"

  # ----- Validate parameters --------------------------------------------------

  NAAQS = match.arg(NAAQS)

  if (!is.data.frame(x)) {
    stop("nowcast_to_aqi() requires a data.frame/tibble of numeric columns.")
  }

  # ----- AQI algorithm --------------------------------------------------------

  # Coerce to numeric matrix (fast, enables vectorized arithmetic).
  # Non-numeric values become NA via coercion.
  mat <- suppressWarnings(as.matrix(x))
  storage.mode(mat) <- "double"

  # Initialize result with NA_integer_
  out <- matrix(NA_integer_, nrow(mat), ncol(mat),
                dimnames = dimnames(mat))

  # Finite mask
  ok <- is.finite(mat)

  # <= 0 -> 0 (only where finite)
  le0 <- ok & (mat <= 0)
  out[le0] <- 0L

  # Positive finite values
  pos <- ok & (mat > 0)
  if (!any(pos)) {
    return(as.data.frame(out, stringsAsFactors = FALSE))
  }

  # Truncate to 0.1 µg/m³ (floor, not round), only for positive values
  mat_trunc <- mat
  mat_trunc[pos] <- floor(mat[pos] * 10) / 10

  # ------------------------------------------------------------------
  # AQI breakpoints (extended AQI) — same as Python
  # ------------------------------------------------------------------
  if ( parameterName == "PM2.5") {

    if ( NAAQS == "PM2.5" ) {

      # TODO:  Revisit and test these breakpoints
      pm_lo  <- c(0.0, 12.001, 35.5, 55.5, 150.5, 250.5, 350.5)
      pm_hi  <- c(12.0, 35.4,  55.4, 150.4, 250.4, 350.4, 500.4)
      aqi_lo <- c(0,   51,    101,  151,  201,  301,  401)
      aqi_hi <- c(50,  100,   150,  200,  300,  400,  500)

    } else if ( NAAQS == "PM2.5_2024") {

      # Extended AQI breakpoints
      pm_lo  <- c(0.0,  9.1,  35.5,  55.5, 125.5, 225.5)
      pm_hi  <- c(9.0, 35.4,  55.4, 125.4, 225.4, 325.4)
      aqi_lo <- c(0,    51,   101,   151,  201,   301)
      aqi_hi <- c(50,   100,  150,   200,  300,   500)

    }

  } else {
    stop("only PM2.5 currently supported")
  }

  # Segment index per cell:
  # - findInterval gives 0..length(pm_hi)
  # - +1 makes it 1..length(pm_hi)+1
  idx <- findInterval(mat_trunc, pm_hi, left.open = TRUE) + 1L

  # Anything above last breakpoint uses the last segment (enables extrapolation)
  idx[idx > length(pm_hi)] <- length(pm_hi)

  # Map per-cell breakpoints via indexed lookup
  BP_Lo <- pm_lo[idx]
  BP_Hi <- pm_hi[idx]
  I_Lo  <- aqi_lo[idx]
  I_Hi  <- aqi_hi[idx]

  denom <- BP_Hi - BP_Lo
  good <- pos & (denom > 0)

  # AQI equation (vectorized over the whole "brick")
  aqi <- (I_Hi - I_Lo) / denom * (mat_trunc - BP_Lo) + I_Lo

  out[good] <- as.integer(round(aqi[good], 0))

  as.data.frame(out, stringsAsFactors = FALSE)
}
