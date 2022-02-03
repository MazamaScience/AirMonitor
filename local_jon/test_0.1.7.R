# Version 0.1.7

# ----- Test latest/daily ------------------------------------------------------

library(AirMonitor)

archiveBaseUrl <- "https://airfire-data-exports.s3.us-west-2.amazonaws.com/monitoring/v2"

wrcc_2021 <- wrcc_loadAnnual(2021, archiveBaseUrl = archiveBaseUrl)

monitor_leaflet(wrcc_2021)


wrcc_2021 %>%
  monitor_select("20a4875c570dc8db_wrcc.s316") %>%
  monitor_trimDate() %>%
  monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE)

wrcc_2021 %>%
  monitor_select("20a4875c570dc8db_wrcc.s316") %>%
  monitor_trimDate(trimEmptyDays = FALSE) %>%
  monitor_timeseriesPlot()

