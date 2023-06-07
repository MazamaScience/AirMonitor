# Cross-walk between v1 and v2 databases

library(AirMonitor)

new <- airnow_loadLatest()
old <- PWFSLSmoke::airnow_loadLatest()

# Identify 9-digit IDS (w/o the extra country code)
new$meta$mask <- stringr::str_length(new$meta$AQSID) == 9

short <- new$meta %>% dplyr::filter(new$meta$mask)
long <- new$meta %>% dplyr::filter(!new$meta$mask)

short$fake_v1_ID <- paste0(short$AQSID, "_01")

long$fake_v1_ID <- paste0(
  long$AQSID %>% stringr::str_replace("^840|^124|^484|^084", ""),
  "_01"
)

v2_v1_lookup <-
  dplyr::bind_rows(short, long) %>%
  dplyr::select(all_of(c(
    "deviceDeploymentID",
    "deviceID",
    "fake_v1_ID",
    "locationName",
    "stateCode",
    "countyName",
    "airnow_agencyName"
  ))) %>%
  dplyr::rename(
    v2_deviceID = .data$deviceID
  )


View(v2_v1_lookup)
