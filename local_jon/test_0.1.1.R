# Version 0.1.1

# ----- Test NowCast -----------------------------------------------------------

library(AirMonitor)

archiveBaseUrl <- "http://data-monitoring_v2-c1.airfire.org/monitoring-v2"

latest <- airnow_loadLatest(archiveBaseUrl = archiveBaseUrl)
latest_nowcast <- latest %>% monitor_nowcast()

nowcast <- airnow_loadLatest("PM2.5_nowcast", archiveBaseUrl = archiveBaseUrl)

j = 655
plot(nowcast$data[,c(1,j)], pch = 15)
points(latest_nowcast$data[,c(1,j)], cex = 1.2, col = "salmon")
title(nowcast$meta$locationName[j - 1])

# NOTE:  Some differences for j = 52,  on 2021-12-23 immediately after missing data
# NOTE:  Some differences for j = 134 on 2021-12-27 immediately after missing data
# NOTE:  Some differences for j = 135 on 2021-12-26 immediately after zero values (lifted by load function?)
# NOTE:  Some differences for j = 138 on 2021-12-22 immediately after missing data
# NOTE:  Some differences for j = 618 on 2021-12-23 immediately after ???


# ----- Explore write GeoJSON --------------------------------------------------

currentTbl <-
  PWFSLSmoke::monitor_getCurrentStatus(
    monitor_toPWFSLSmoke(latest),
    lubridate::now(tzone = "UTC")
  )

geojsonio::map_leaf(latest$meta)

###geojsonio::geojson_write(latest$meta, file = "bop.geojson")

desiredColumns <- c(
  "deviceDeploymentID",
  "dataIngestSource",
  "locationName",
  "longitude",
  "latitude",
  "timezone",
  "AQSID"
)

latest$meta %>%
  dplyr::select(dplyr::all_of(desiredColumns)) %>%
  geojsonio::geojson_write(
    lat = "latitude",
    lon = "longitude",
    file = "bop.geojson",
    overwrite = TRUE,
    precision = 4
  )


# This produces:
#
# { "type": "Feature", "properties": { "deviceDeploymentID": "0001770edc620bb8_000052001", "deviceID": "000052001", "deviceType": null, "deviceDescription": null, "deviceExtra": null, "pollutant": "PM2.5", "units": "UG/M3", "dataIngestSource": "AirNow", "dataIngestURL": "https://www.airnowapi.org/aq/data/", "dataIngestUnitID": null, "dataIngestExtra": null, "dataIngestDescription": null, "locationID": "0001770edc620bb8", "locationName": "Charette", "longitude": -72.8928, "latitude": 46.4425, "elevation": 116.8, "countryCode": "CA", "stateCode": "QC", "countyName": null, "timezone": "America/Toronto", "houseNumber": null, "street": null, "city": null, "zip": null, "AQSID": "000052001", "airnow_parameterName": "PM2.5", "airnow_siteCode": "2001", "airnow_status": "Active", "airnow_agencyID": "QC1", "airnow_agencyName": "Canada-Quebec1", "airnow_EPARegion": "CA", "airnow_GMTOffsetHours": -5.0, "airnow_FIPSMSACode": null, "airnow_MSAName": null }, "geometry": { "type": "Point", "coordinates": [ -72.8928, 46.4425 ] } }


# v4 geojson:
#
# {
#   "type":"Feature",
#   "id":1,
#   "properties":{
#     "monitorID":"lon_.123.394_lat_41.790_arb2.1049",
#     "siteName":"Happy Camp-770 Lower Airport Rd",
#     "agencyName":null,
#     "timezone":"America/Los_Angeles",
#     "dataSource":"AIRSIS",
#     "telemetryAggregator":"arb2.airsis",
#     "telemetryUnitID":"1049",
#     "processingTime":"2021-12-30 00:28:00",
#     "lastValidUTCTime":"2021-12-27 23:00:00",
#     "lastValidLocalTime":"2021-12-27 23:00:00 UTC",
#     "PM2.5_nowcast":null,
#     "PM2.5_1hr":8,
#     "PM2.5_3hr":8,
#     "PM2.5_yesterday":null,
#     "latency":48,
#     "monitoringSiteUrl":"http://tools.airfire.org/monitoring/v4/#!/?monitors=lon_.123.394_lat_41.790_arb2.1049"
#   },
#   "geometry":{"type":"Point","coordinates":[-123.3945,41.79]}
# }

# Stuart DOESN'T NEED: monitoringSiteUrl, processingTime, telemetryUnitID, timezone, telemetryAggregator, latency, PM2.5_3hr, PM2.5_yesterday
# Stuart NEEDS: sensor_id,lat,lng,utc_ts,nowcast_pm25,epa_pm25,timezone,raw_pm25

# Mv4 NEEDS:  monitorID, siteNam,e, timezone, dataSource, ...
