# Find diurnal data and do a statistical analysis of how day boundary time
# affects calculation of daily averages

# NOTE:  Using PWFSLSmoke until we have archival v2 data

library(AirMonitor)
library(MazamaRollUtils)

# ----- Download data ----------------------------------------------------------

AirNow_2016 <-
  PWFSLSmoke::airnow_loadAnnual(2016) %>%
  monitor_fromPWFSLSmoke()


# ----- What does a diurnal signal look like -----------------------------------

AirMonitor::Carmel_Valley %>%
  monitor_toPWFSLSmoke() %>%
  AirMonitorPlots::monitor_ggDailyByHour_archival(
    title = "Carmel Valley -- Sobranes Fire"
  )


# ----- "Diurnicity:  Carmel Valley --------------------------------------------

Carmel_Valley <-
  AirNow_2016 %>%
  monitor_filter(AQSID == "060530002")

timezone <- Carmel_Valley$meta$timezone
t <- Carmel_Valley$data$datetime
x <- Carmel_Valley$data[,2]

a <- .bincode(x, breaks = US_AQI$breaks_PM2.5, right = FALSE)

a_max <- roll_max(a, 24, align = "left", na.rm = TRUE)
a_mean <- roll_mean(a, 24, align = "left", na.rm = TRUE)
a_min <- roll_min(a, 24, align = "left", na.rm = TRUE)

diurnicity <- 10 * a_mean * (a_max - a_min)

layout(matrix(seq(1:8), ncol = 2, byrow = TRUE))

for ( i in seq(0,14,2) ) {

  startdate <- MazamaCoreUtils::parseDatetime(20160701, timezone = timezone) + lubridate::dweeks(i)
  enddate <- startdate + lubridate::dweeks(2)

  Carmel_Valley %>%
    monitor_filterDate(startdate, enddate) %>%
    monitor_timeseriesPlot(shadedNight = TRUE, pch = 15, cex = 0.5, ylim = c(0, 200))

  addAQIStackedBar(palette = "deuteranopia")

  # diurnicity ranges goes up to 20+ so multiply by 10 to plot
  lines(t, diurnicity, lwd = 3, col = adjustcolor("coral", 0.5))
  abline(h = 20, lty = "dashed")

  legend(
    "topright",
    legend = "'diurnicity'",
    lwd = 3,
    col = adjustcolor("coral", 0.5)
  )

}

layout(1)


# ----- Carmel Valley July-August ----------------------------------------------

Carmel_Valley_JA <-
  Carmel_Valley %>%
  monitor_filterDate(20160701, 20160901, timezone = "America/Los_Angeles")

t_JA <- Carmel_Valley_JA$data$datetime
x_JA <- Carmel_Valley_JA$data[,2]

a_JA <- .bincode(x_JA, breaks = US_AQI$breaks_PM2.5, right = FALSE)

a_JA_max <- roll_max(a_JA, 24, align = "left", na.rm = TRUE)
a_JA_mean <- roll_mean(a_JA, 24, align = "left", na.rm = TRUE)
a_JA_min <- roll_min(a_JA, 24, align = "left", na.rm = TRUE)

diurnicity_JA <- 10 * a_JA_mean * (a_JA_max - a_JA_min)

layout(matrix(seq(3)))

# Annual w/ cutoff
Carmel_Valley_JA %>%
  monitor_timeseriesPlot(shadedNight = FALSE, pch = 15, cex = 1.0, ylim = c(0, 400))

addAQIStackedBar(palette = "deuteranopia")

# diurnicity ranges goes up to 20+ so multiply by 10 to plot
lines(t_JA, diurnicity_JA, lwd = 3, col = adjustcolor("coral", 0.5))
abline(h = 20, lty = "dashed")

# Annual w/ all hours
Carmel_Valley_JA %>%
  monitor_timeseriesPlot(shadedNight = FALSE, pch = 15, cex = 0.8, ylim = c(0,400))

diurnal_mask <- diurnicity_JA > 20
points(t_JA[diurnal_mask], x_JA[diurnal_mask], pch = 15, cex = 0.8, col = 'red')

addAQIStackedBar(palette = "deuteranopia")

# Annual w/ all maxes
Carmel_Valley_JA %>%
  monitor_timeseriesPlot(shadedNight = FALSE, cex = 0.8, ylim = c(0, 400))

###points(t_JA[diurnal_mask], x_JA[diurnal_mask], pch = 15, cex = 0.8, col = adjustcolor('red', 0.5))

rolling_max_mask <- x_JA == roll_max(x_JA, 24, align = "center", na.rm = TRUE)
diurnal_max_mask <- diurnal_mask & rolling_max_mask

points(t_JA[diurnal_max_mask], x_JA[diurnal_max_mask], pch = 1, cex = 1.0, lwd = 2, col = 'red')

addAQIStackedBar(palette = "deuteranopia")

layout(1)

# ----- What hours are involved ------------------------------------------------


diurnal_max_hour_of_day <-
  t_JA[diurnal_max_mask] %>%
  na.omit() %>%
  lubridate::with_tz(timezone) %>%
  lubridate::hour()

hist(
  diurnal_max_hour_of_day,
  right = FALSE,
  xlim = c(0, 24),
  main = "Hour of Daily Max During Jul-Aug 'Diurnicity'",
  xlab = "Local time",
  axes = FALSE
)
segments(0, 0, 24, 0)
axis(1, at = c(0, 6, 12, 18, 24), labels = c("midnight", "6 AM", "noon", "6 PM", "midnight"))
axis(2, las = 1)












lo_index <- min(which(diurnicity > 20)) - 2 * 24
hi_index <- min(which(diurnicity > 20)) + 12 * 24
lo_to_hi <- lo_index:hi_index

# Narrow things down to the strongly diurnal part

monitor_timeseriesPlot(
  Carmel_Valley,
  style = 'gnats',
  tlim = c(t[lo_index], t[hi_index]),
  shadedNight = TRUE
)
addAQIStackedBar()
lines(t[lo_to_hi], diurnicity[lo_to_hi], lwd = 2, col = adjustcolor("coral", 0.5))
abline(h = 20)

# Does diurnicity ignore periods of non-diurnal high smoke?

# Yuba City, Camp Fire

Camp_Fire <-
  airnow_loadAnnual(2018) %>%
  monitor_subset(stateCodes = "CA", tlim = c(20181101, 20181201))

Yuba_City <-
  Camp_Fire %>%
  monitor_subset(monitorID = "061010003_01")

monitor_timeseriesPlot(Yuba_City, style = "gnats")

t <- Yuba_City$data$datetime
x <- Yuba_City$data[,2]

a <- cut(x, breaks = AQI$breaks_24) %>% as.numeric()
a_max <- roll_max(a, 24, na.rm = TRUE)
a_mean <- roll_mean(a, 24, na.rm = TRUE)
a_min <- roll_min(a, 24, na.rm = TRUE)

diurnicity <- a_mean * (a_max - a_min)

monitor_timeseriesPlot(Yuba_City, style = 'gnats', shadedNight = TRUE)
addAQIStackedBar()
lines(t, diurnicity*10, lwd = 2, col = adjustcolor("coral", 0.5))
abline(h = 20)

# Downtown Sacramento

Downtown_Sacramento <-
  Camp_Fire %>%
  monitor_subset(monitorID = "060670010_01")

monitor_timeseriesPlot(Downtown_Sacramento, style = "gnats")

t <- Downtown_Sacramento$data$datetime
x <- Downtown_Sacramento$data[,2]

a <- cut(x, breaks = AQI$breaks_24) %>% as.numeric()
a_max <- roll_max(a, 24, na.rm = TRUE)
a_mean <- roll_mean(a, 24, na.rm = TRUE)
a_min <- roll_min(a, 24, na.rm = TRUE)

diurnicity <- a_mean * (a_max - a_min)

monitor_timeseriesPlot(Downtown_Sacramento, style = 'gnats', shadedNight = TRUE)
addAQIStackedBar()
lines(t, diurnicity*10, lwd = 2, col = adjustcolor("coral", 0.5))
abline(h = 20)

# Ukiah

Ukiah <-
  Camp_Fire %>%
  monitor_subset(monitorID = "060450006_01")

monitor_timeseriesPlot(Ukiah, style = "gnats")

t <- Ukiah$data$datetime
x <- Ukiah$data[,2]

a <- cut(x, breaks = AQI$breaks_24) %>% as.numeric()
a_max <- roll_max(a, 24, na.rm = TRUE)
a_mean <- roll_mean(a, 24, na.rm = TRUE)
a_min <- roll_min(a, 24, na.rm = TRUE)

diurnicity <- a_mean * (a_max - a_min)

monitor_timeseriesPlot(Ukiah, style = 'gnats', shadedNight = TRUE)
addAQIStackedBar()
lines(t, diurnicity*10, lwd = 2, col = adjustcolor("coral", 0.5))
abline(h = 20)

# Seely Lake, 2017

Seely_Lake <-
  airnow_loadAnnual(2017) %>%
  monitor_subset(monitorIDs = "300630038_01")

t <- Seely_Lake$data$datetime
x <- Seely_Lake$data[,2]

a <- cut(x, breaks = AQI$breaks_24) %>% as.numeric()
a_max <- roll_max(a, 24, na.rm = TRUE)
a_mean <- roll_mean(a, 24, na.rm = TRUE)
a_min <- roll_min(a, 24, na.rm = TRUE)

diurnicity <- a_mean * (a_max - a_min)

monitor_timeseriesPlot(Seely_Lake, style = 'gnats', tlim = c(20170801,20170901), shadedNight = TRUE)
addAQIStackedBar()
lines(t, diurnicity*10, lwd = 2, col = adjustcolor("coral", 0.5))
abline(h = 20)

