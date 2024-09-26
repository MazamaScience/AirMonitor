# Kelly Martin -- 2023-09-06
#
# I am getting this error when trying to run the following code (just trying to run the example code before using with my own data questions):
# camp_fire <-
#   monitor_loadAnnual(2018) %>%
#   monitor_subset(stateCodes = 'CA') %>%
#   monitor_subset(tlim = c(20181108, 20181123))
# " Error: data file could not be loaded from: https://haze.airfire.org/monitoring/AirNow/RData/2018/airnow_PM2.5_2018.RData"
#

packageVersion("AirMonitor")
# [1] ‘0.3.12’

library(AirMonitor)

camp_fire <-
  monitor_loadAnnual(2018) %>%
  monitor_filter(stateCode == 'CA') %>%
  monitor_filterDate(20181108, 20181123)

dim(camp_fire$meta)

