library(AirMonitor)

a <-
  epa_aqs_loadAnnual(
    2015,
    archiveBaseDir = "~/Projects/AirFire/monitoring-data-ingest-v2/test/data"
  ) %>%
  monitor_filter(stateCode %in% c("WA", "OR", "ID"))

b <-
  epa_aqs_loadAnnual(
    2015,
    archiveBaseDir = "~/Projects/AirFire/monitoring-data-ingest-v2/test/data",
    parameterCode = "88101"
  ) %>%
  monitor_filter(stateCode %in% c("WA", "OR", "ID"))

c <- monitor_combine(a, b)

NW_Megafires <-
  c %>%
  monitor_filterDate(20150724, 20150907) %>%
  monitor_dropEmpty()


# mts_combine ussue (needs trim() applied)

a <-
  epa_aqs_loadAnnual(
    2015,
    archiveBaseDir = "~/Projects/AirFire/monitoring-data-ingest-v2/test/data",
    parameterCode = "88502"
  )

b <-
  epa_aqs_loadAnnual(
    2015,
    archiveBaseDir = "~/Projects/AirFire/monitoring-data-ingest-v2/test/data",
    parameterCode = "88101"
  )

a1 <- a %>% monitor_filter(deviceID == "060670010_03")
b1 <- b %>% monitor_filter(deviceID == "060670010_03")

# Separate parts of the year
a1 %>% monitor_timeseriesPlot()
b1 %>% monitor_timeseriesPlot(col = "salmon", add = TRUE)

# But combine fails without first trimming the data
c <- monitor_combine(list(a, b), replaceMeta = TRUE)
c %>% monitor_filter(deviceID == "060670010_03") %>% monitor_timeseriesPlot()

