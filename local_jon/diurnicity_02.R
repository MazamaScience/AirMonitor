# Find diurnal data and do a statistical analysis of how day boundary time
# affects calculation of daily averages

# NOTE:  Using PWFSLSmoke until we have archival v2 data

library(PWFSLSmoke)
library(MazamaRollUtils)

# Carmel Valley, Sobranes Fire, STRONGLY diurnal
Carmel_Valley <-
  airnow_loadAnnual(2016) %>%
  monitor_subset(monitorIDs = "060530002_01")

t <- Carmel_Valley$data$datetime
x <- Carmel_Valley$data[,2]

x[x == 0] <- 0.1
a <- log(x)
a[a < 0] <- 0.1
a_max <- roll_max(a, 24, na.rm = TRUE)
a_mean <- roll_mean(a, 24, na.rm = TRUE)
a_min <- roll_min(a, 24, na.rm = TRUE)

diurnicity <- 1 * (a_max - a_min)

monitor_timeseriesPlot(Carmel_Valley, style = 'gnats')
addAQIStackedBar()
lines(t, diurnicity * 20, lwd = 2, col = adjustcolor("coral", 0.5))
###abline(h = 20)



################################################################################
################################################################################
################################################################################















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
x_mean <- roll_mean(x, 24, na.rm = TRUE)

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
x_mean <- roll_mean(x, 24, na.rm = TRUE)

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
x_mean <- roll_mean(x, 24, na.rm = TRUE)

a <- cut(x, breaks = AQI$breaks_24) %>% as.numeric()
a_max <- roll_max(a, 24, na.rm = TRUE)
a_mean <- roll_mean(a, 24, na.rm = TRUE)
a_min <- roll_min(a, 24, na.rm = TRUE)

diurnicity <- a_mean * (a_max - a_min)

monitor_timeseriesPlot(Ukiah, style = 'gnats', shadedNight = TRUE)
addAQIStackedBar()
lines(t, diurnicity*10, lwd = 2, col = adjustcolor("coral", 0.5))
abline(h = 20)

# Should we calculate diurnicity based on a log scale? Does the distribution
# of numbers or the health impacts support that?
