# ASIC 2022 Tutorial for the AirMonitor package

library(AirSensor)

# This demo uses pre-processed files availavle from this archive
setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")

# Load a Purple Air Synoptic  file (all sensor locations)
pas <- pas_load()

# Lots of columns
dplyr::glimpse(pas)

# NOTE:  This archive contains historical data processed for SCAQMD. These
# NOTE:  sensors can be identified because they have had a "communityRegion" assigned.

scaqmd <-
  pas %>%
  pas_filter(!is.na(communityRegion))

# Map the SCAQMD locations

# NOTE:  The map is NOT real-time. We just need it to get monitor IDs
scaqmd %>% pas_leaflet()


# Click on the map to read off a deviceDeploymentID to use as the "id"

# Load archival data for November 2021
pat <-
  pat_load(
    id = "173ff64a55da1183_2693",
    pas = pas,
    startdate = "2021-11-01",
    enddate = "2021-12-01"
  )

# Plot the data
pat %>% pat_multiplot()

# Just the A and B channel PM2.5
pat %>% pat_multiplot("pm25_over")

# Performa an "internal fit"
pat %>% pat_internalFit()

# Perform an "external fit" with the closest regulatory monitor
pat %>% pat_externalFit()
