# Jim Soldenwanger Yosemite monitors
#
# monitor <- airnow_loadLatest()
# myvector <- c("9f7489899d373324_840MMFS13009", "babddc7866d7c4a8_840MMFS11092", "1659635e902dfcc4_840MMFS11094")
# monitor_ggDailyHourlyBarplot(monitor, id = myvector)

library(AirMonitor)
library(AirMonitorPlots)

airnow <- airnow_loadLatest()

# Try `names(airnow$meta)` to see all metadata fields

# These are the AirNow unique identifiers from `myvector` above
fullAQSIDs <- c("840MMFS13009", "840MMFS11092", "840MMFS11094")

# Filter based on this field in the metadata
monitor <-
  airnow %>%
  monitor_filter(
    fullAQSID %in% fullAQSIDs
  )

# Visual verification
monitor %>% monitor_leaflet()

# Pull out the deviceDeploymentIDs
IDs <- monitor %>% monitor_pull("deviceDeploymentID")

# > IDs
# [1] "1659635e902dfcc4_840MMFS11094" "9f7489899d373324_840MMFS13009" "babddc7866d7c4a8_840MMFS11092"
#
# NOTE:  The "v2" database has different locationIDs than the "v1" database

# Plot
monitor_ggDailyHourlyBarplot(monitor, id = IDs)



