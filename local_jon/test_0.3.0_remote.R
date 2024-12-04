# Version 0.5.x

library(AirMonitor)

# ===== LATEST =================================================================

# ----- Test v2 database compatibility -----------------------------------------

archiveBaseUrl <- "https://airfire-data-exports.s3.us-west-2.amazonaws.com/monitoring/v2"

airnow_latest <- airnow_loadLatest(archiveBaseUrl = archiveBaseUrl)

# Check New Mexico
airnow_latest %>%
  monitor_filter(stateCode == "NM") %>%
  monitor_leaflet()

# Taos, NM
airnow_latest %>%
  monitor_select("1958e8a372add8da_840350550005") %>%
  monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE)


# ----- Test airnow_latest PM2.5 -----------------------------------------------

archiveBaseUrl <- "https://airfire-data-exports.s3.us-west-2.amazonaws.com/monitors/v3"

airnow_latest <- airnow_loadLatest(archiveBaseUrl = archiveBaseUrl)

# Is the airnow data using fullAQSID as the deviceID? If so, all should be 12 chars long.
table(stringr::str_length(airnow_latest$meta$deviceID))

# Deployment type should be known for all monitors in sites_meta
any(is.na(airnow_latest$meta$deploymentType))

# Are we lifting up negative values?
any(airnow_latest$data[,-1] < 0, na.rm = TRUE) # Should be FALSE

# Do we have any duplicated locations?
any(duplicated(airnow_latest$meta$locationID)) # Should be FALSE ???

# Check non-US
airnow_latest %>%
  monitor_filter(!countryCode %in% c("CA", "US", "MX")) %>%
  monitor_leaflet()

# Check New Mexico
airnow_latest %>%
  monitor_filter(stateCode == "NM") %>%
  monitor_leaflet()

# Taos, NM
airnow_latest %>%
  monitor_select("9wkyvrk_840350550005") %>%
  monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE)


# ----- Test airnow_latest CO --------------------------------------------------

airnow_latest <-
  airnow_loadLatest(
    archiveBaseUrl = archiveBaseUrl,
    parameterName = "CO"
  )

# How many monitors?
nrow(airnow_latest$meta)

airnow_latest %>% monitor_leaflet()

# Sur, Mexico
airnow_latest %>%
  monitor_select("9ezk22c_484800010001") %>%
  monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE, ylim = c(0, 20))


# ----- Test airnow_latest PM10 ------------------------------------------------

airnow_latest <-
  airnow_loadLatest(
    archiveBaseUrl = archiveBaseUrl,
    parameterName = "PM10"
  )


# ===== DAILY ==================================================================

# ----- Test airnow_daily ------------------------------------------------------

airnow_daily <- airnow_loadDaily(archiveBaseUrl = archiveBaseUrl)

# Do we have any duplicated locations?
any(duplicated(airnow_daily$meta$locationID)) # Should be FALSE



# ----- Test airsis_latest -----------------------------------------------------

airsis_latest <- airsis_loadLatest(archiveBaseUrl = archiveBaseUrl)

airsis_latest %>%
  monitor_select("5dee0af2d2745180_usfs.1073") %>%
  monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE)

# Some AIRSIS monitors are found in the AirNow database and have fullAQSID
airsis_latest$meta$fullAQSID

# ----- Test airsis_latest -----------------------------------------------------

airsis_daily <- airsis_loadDaily(archiveBaseUrl = archiveBaseUrl)


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

