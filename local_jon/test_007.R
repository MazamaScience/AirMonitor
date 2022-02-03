# Version 0.0.7

library(AirMonitor)

archiveBaseUrl <- "https://airfire-data-exports.s3.us-west-2.amazonaws.com/monitoring/v2"

l1 <- airnow_loadLatest(archiveBaseUrl = archiveBaseUrl)
l2 <- airsis_loadLatest(archiveBaseUrl = archiveBaseUrl)
l3 <- wrcc_loadLatest(archiveBaseUrl = archiveBaseUrl)

###d1 <- airnow_loadDaily(archiveBaseUrl = archiveBaseUrl)
d2 <- airsis_loadDaily(archiveBaseUrl = archiveBaseUrl)
d3 <- wrcc_loadDaily(archiveBaseUrl = archiveBaseUrl)

# ----- Check on QC of negative values -----

layout(matrix(seq(1:3), nrow = 1))

airsis_loadDaily(archiveBaseUrl = archiveBaseUrl, QC_negativeValues = "ignore") %>%
  monitor_timeseriesPlot(ylim=c(-20,50), xpd = NA, main = "QC 'ignore'")

airsis_loadDaily(archiveBaseUrl = archiveBaseUrl, QC_negativeValues = "na") %>%
  monitor_timeseriesPlot(ylim=c(-20,50), xpd = NA, main = "QC 'na'")

airsis_loadDaily(archiveBaseUrl = archiveBaseUrl, QC_negativeValues = "zero") %>%
  monitor_timeseriesPlot(ylim=c(-20,50), xpd = NA, main = "QC 'zero'")

layout(1)

# ----- Daily averages -----

daily_d2 <- monitor_dailyStatistic(
  monitor = d2,
  FUN = mean,
  na.rm = TRUE,
  minHours = 18,
  dayBoundary = c("clock")
)

head(daily_d2$data)

# ----- Daily threshold -----

Carmel_Valley %>%
  monitor_dailyThreshold("very unhealthy") %>%
  monitor_getData()
