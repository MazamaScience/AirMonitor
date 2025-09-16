library(AirMonitor)

airnow <- airnow_loadAnnual(2022)

airnow %>%
  monitor_filterDate(20220628, 20220701, timezone = "UTC") %>%
  monitor_timeseriesPlot()

airnow_m <- airnow_loadMonthly(202206)

airnow_m %>%
  monitor_filterDate(20220628, 20220701, timezone = "UTC") %>%
  monitor_timeseriesPlot()

