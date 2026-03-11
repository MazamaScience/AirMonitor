library(AirMonitor)
library(AirMonitorPlots)

monitor_2025 <- monitor_loadAnnual(2025)

# Garnet Fire
ca <-
  monitor_2025 %>%
  monitor_filter(stateCode == 'CA') %>%
  monitor_filterDate(20250820, 20250920)

Fish_Lake_id <- "0da1e85464a22ed9_840MMFS11093"

Fish_Lake <-
  ca %>%
  monitor_select(Fish_Lake_id)

Fish_Lake %>%
  monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE)

Fish_Lake %>%
  monitor_ggTimeseries_archival()

Fish_Lake %>%
  monitor_ggDailyByHour_archival()

Fish_Lake %>%
  monitor_ggDailyHourlyBarplot(
    id = Fish_Lake_id
  )

