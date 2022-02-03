# Version 0.1.2

# ----- Test NowCast -----------------------------------------------------------

library(AirMonitor)

archiveBaseUrl <- "https://airfire-data-exports.s3.us-west-2.amazonaws.com/monitoring/v2"

latest <- airnow_loadLatest(archiveBaseUrl = archiveBaseUrl)
latest_nowcast <- latest %>% monitor_nowcast()

nowcast <- airnow_loadLatest("PM2.5_nowcast", archiveBaseUrl = archiveBaseUrl)

j = 655
plot(latest$data[,c(1,j)], pch = 15, cex = 0.5, col = "salmon")
points(nowcast$data[,c(1,j)], pch = 15, col = "dodgerblue")
points(latest_nowcast$data[,c(1,j)], cex = 1.2, col = "black")
title(nowcast$meta$locationName[j - 1])

# NOTE:  Some differences for j = 52,  on 2021-12-23 immediately after missing data
# NOTE:  Some differences for j = 134 on 2021-12-27 immediately after missing data
# NOTE:  Some differences for j = 135 on 2021-12-26 immediately after zero values (lifted by load function?)
# NOTE:  Some differences for j = 138 on 2021-12-22 immediately after missing data
# NOTE:  Some differences for j = 618 on 2021-12-23 immediately after ???


# ----- Explore write GeoJSON --------------------------------------------------

pt <- proc.time()
currentTbl <-
  PWFSLSmoke::monitor_getCurrentStatus(
    monitor_toPWFSLSmoke(latest),
    lubridate::now(tzone = "UTC")
  )
print(proc.time() - pt)

###geojsonio::map_leaf(latest$meta)

###geojsonio::geojson_write(latest$meta, file = "bop.geojson")


pt <- proc.time()
currentTbl <- monitor_getCurrentStatus(latest)
print(proc.time() - pt)



desiredColumns <- c(
  "longitude",
  "latitude",
  "deviceDeploymentID",
  "AQSID",
  "locationName",
  "timezone",
  "dataIngestSource",
  "dataIngestUnitID",
  "currentStatus_processingTime",
  "last_validTime",
  "last_validLocalTimestamp",
  "last_nowcast",
  "last_PM2.5",
  "last_latency",
  "yesterday_PM2.5_avg"
)


# File -- unquoted numeric values
currentTbl %>%
  dplyr::select(dplyr::all_of(desiredColumns)) %>%
  geojsonio::geojson_write(
    lat = "latitude",
    lon = "longitude",
    geometry = "point",
    group = NULL,
    file = "myfile.geojson",
    overwrite = TRUE,
    precision = 5,
    convert_wgs84 = FALSE,
    crs = NULL,
  )


 # String -- quoted numeric values
geojsonString <-
  currentTbl %>%
  dplyr::select(dplyr::all_of(desiredColumns)) %>%
  geojsonio::geojson_json(
    lat = "latitude",
    lon = "longitude",
    group = NULL,
    geometry = "point",
    type = "FeatureCollection",
    convert_wgs84 = FALSE,
    crs = NULL,
    precision = 5,
    null = "null",
    na = "null"
  )



# This produces:
#
# {
#   "type": "Feature",
#   "properties": {
#     "longitude": -72.8928,
#     "latitude": 46.4425, "deviceDeploymentID":
#     "0001770edc620bb8_000052001",
#     "AQSID": "000052001",
#     "locationName": "Charette",
#     "timezone": "America/Toronto",
#     "dataIngestSource": "AirNow",
#     "dataIngestUnitID": null,
#     "currentStatus_processingTime": "2022-01-03T18:05:11.825Z",
#     "last_validTime": "2022-01-03T15:00:00Z",
#     "last_validLocalTimestamp": "2022-01-03 07:00:00 PST",
#     "last_nowcast": 3.1,
#     "last_PM2.5": 3.0,
#     "last_latency": 1.0,
#     "yesterday_PM2.5_avg": 2.4 },
#     "geometry": { "type": "Point", "coordinates": [ -72.8928, 46.4425 ] }
# }

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
