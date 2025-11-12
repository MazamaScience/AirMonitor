# Making sure everything works with the 'daily' files

library(AirMonitor)

latest <- monitor_loadLatest()
daily <- monitor_loadDaily()
annual_2025 <- monitor_loadAnnual(2025)

latest %>% monitor_timeRange()
daily %>% monitor_timeRange()
annual_2025 %>% monitor_timeRange()

annual_2025 %>%
  monitor_filter(stateCode == "IA") %>%
  monitor_filterDate(20250601, 20251006) %>%
  monitor_timeseriesPlot()

mon <- monitor_load(20251010, 20251028)


mon %>% monitor_filter(stateCode == "IA") %>% monitor_timeseriesPlot()
