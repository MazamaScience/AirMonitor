# Making sure everything works with the 'daily' files

library(AirMonitor)

latest <- airnow_loadLatest()
daily <- airnow_loadDaily()
annual_2024 <- airnow_loadAnnual(2024)
annual_2025 <- airnow_loadAnnual(2025)
monthly_202506 <- airnow_loadMonthly(202506)
monthly_202507 <- airnow_loadMonthly(202507)
monthly_202508 <- airnow_loadMonthly(202508)

latest %>% monitor_timeRange()
daily %>% monitor_timeRange()
annual_2024 %>% monitor_timeRange()
annual_2025 %>% monitor_timeRange()
monthly_202506 %>% monitor_timeRange()

mon <- monitor_load(20250601, 20251006)

