# Version 0.3.0

# ----- Test airnow_latest -----------------------------------------------------

library(AirMonitor)

archiveBaseUrl <- "http://52.25.138.138/monitoring-v2"

airnow_latest <- airnow_loadLatest(archiveBaseUrl = archiveBaseUrl)

# Is the airnow data using fullAQSID as the deviceID? If so, all should be 12 chars long.
table(stringr::str_length(airnow_latest$meta$deviceID))

# Deployment type should be known for all monitors in sites_meta
any(is.na(airnow_latest$meta$deploymentType))
monitor_filterMeta(airnow_latest, is.na(deploymentType)) %>% monitor_leaflet()

# Are we lifting up negative values?
any(airnow_latest$data[,-1] < 0, na.rm = TRUE) # Should be FALSE

# Check New Mexico
airnow_latest %>%
  monitor_filter(stateCode == "NM") %>%
  monitor_leaflet()

# Anthony, NM
airnow_latest %>%
  monitor_select("c733850d59531f1e_840350130016") %>%
  monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE)

# ----- Test airsis_latest -----------------------------------------------------

airsis_latest <- airsis_loadLatest(archiveBaseUrl = archiveBaseUrl)

airsis_latest %>%
  monitor_select("5dee0af2d2745180_usfs.1073") %>%
  monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE)

# Some AIRSIS monitors are found in the AirNow database and have fullAQSID
airsis_latest$meta$fullAQSID

# ----- Test wrcc_latest -------------------------------------------------------

wrcc_latest <- wrcc_loadLatest(archiveBaseUrl = archiveBaseUrl)

wrcc_latest %>% monitor_leaflet()

wrcc_latest %>%
  monitor_select("9817acec26321e5b_wrcc.eat2") %>%
  monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE)

# Some WRCC monitors are found in the AirNow database and have fullAQSID
wrcc_latest$meta$fullAQSID

# Is this data found in both datasets?
airnow_FS10386 <-
  airnow_latest %>%
  monitor_filter(fullAQSID == "840MMFS10386")

wrcc_FS10386 <-
  wrcc_latest %>%
  monitor_filter(fullAQSID == "840MMFS10386")

monitor_timeseriesPlot(airnow_FS10386)
monitor_timeseriesPlot(wrcc_FS10386, pch = 1, col = 'red', add = TRUE)
# Nice! Almost a perfect match

# Look for lon/lats as "named" numerics (Should NOT see any.)
lapply(wrcc_latest$meta, attributes) %>% str()

# ...
# $ locationName         : NULL
# $ longitude            :List of 1
# ..$ names: chr [1:15] "" "" "" "" ...
# $ latitude             :List of 1
# ..$ names: chr [1:15] "" "" "" "" ...
# $ elevation            : NULL
# ...

# ----- Test wrcc_daily --------------------------------------------------------

wrcc_daily <- wrcc_loadDaily(archiveBaseUrl = archiveBaseUrl)

wrcc_daily %>% monitor_leaflet()

wrcc_daily %>%
  monitor_select("7fcd82c324dda94f_wrcc.s328") %>%
  monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE)

