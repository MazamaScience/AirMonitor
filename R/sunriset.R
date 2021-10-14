".balanceCrdsTimes" <- function(crds, dateTime)
{
  ## Value: list with crds and dateTime input matrices with equal number
  ## of rows
  ## --------------------------------------------------------------------
  ## Arguments: crds=matrix with lon and lat; dateTime=matrix with year,
  ## month, day, timezone, and dlstime rows, or a POSIXct time
  ## --------------------------------------------------------------------
  ncrds <- nrow(crds)
  nTimes <- ifelse(is(dateTime, "POSIXct"), length(dateTime), nrow(dateTime))
  if (ncrds == 1 && nTimes > 1) {
    crds <- crds[rep(1, nTimes), ]
  } else if (ncrds > 1 && nTimes == 1) {
    dateTime <- if (is(dateTime, "POSIXct")) {
      dateTime[rep(1, ncrds)]
    } else dateTime[rep(1, ncrds), ]
  } else if (ncrds != nTimes) {
    stop("mismatch in number of coordinates and times")
  }
  list(crds=crds, dateTime=dateTime)
}



sunriset <- function(
  crds,
  dateTime,
  direction=c("sunrise", "sunset"),
  POSIXct.out=FALSE
) {

  crdsmtx <- matrix(c(coordinates(crds)[, 1],
                      coordinates(crds)[, 2]), ncol=2)
  eq.ll <- .balanceCrdsTimes(crdsmtx, dateTime)
  time.ll <- .timeData(eq.ll$dateTime)
  lon <- eq.ll$crds[, 1]
  lat <- eq.ll$crds[, 2]
  direction <- match.arg(direction)
  res <- .sunriset(lon=lon, lat=lat, year=time.ll$year,
                   month=time.ll$month, day=time.ll$day,
                   timezone=time.ll$timezone,
                   dlstime=time.ll$dlstime,
                   direction=direction)
  if (POSIXct.out) {
    secs <- res * 86400
    if (is.null(time.ll$tz)) Pct <- as.POSIXct(format(dateTime,
                                                      "%Y-%m-%d")) + secs
    else Pct <- as.POSIXct(format(dateTime, "%Y-%m-%d"),
                           tz=time.ll$tz) + secs
    res <- data.frame(day_frac=res, time=Pct)
  }
  res

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  crds <- matrix(c(-122.33, 47.60), 1, 2)
  dateTime <- MazamaCoreUtils::parseDatetime(lubridate::now(), timezone = "America/Los_Angeles")
  direction <- "sunrise"
  POSIXct.out = TRUE


  sunriset(
    crds,
    dateTime,
    direction,
    POSIXct.out
  )

}
