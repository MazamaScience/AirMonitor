# Create data for the community-smoke-page prototype

library(AirMonitor)

dataDir <- "~/Downloads"

all_monitors <-
  monitor_loadLatest() %>%
  monitor_filter(stateCode == "WA", countyName == "Okanogan")

meta <- all_monitors$meta
data <- all_monitors$data

readr::write_csv(meta, file = file.path(dataDir, "all_monitors_meta.csv"))
readr::write_csv(data, file = file.path(dataDir, "all_monitors_data.csv"))

