# Susan_ONeill 2025-05-30
#
# ----- Susan's code -----------------------------------------------------------

library(AirMonitor)

yr <- "2024"

yyyymmdd_st <- paste (yr, "0601", sep = "")
yyyymmdd_en <- paste (yr, "1201", sep = "")


allmonitors_24hr_Eastern <- monitor_loadAnnual(year = yr) %>%
  #monitor_filterDate(startdate = yyyymmdd_st, enddate = yyyymmdd_en) %>%
  monitor_filter(timezone == "America/New_York") %>%
  monitor_filter(stateCode %in% CONUS) %>%
  monitor_dailyStatistic()

allmonitors_24hr_Central <- monitor_loadAnnual(year = yr) %>%
  #monitor_filterDate(startdate = yyyymmdd_st, enddate = yyyymmdd_en) %>%
  monitor_filter(timezone == "America/Chicago") %>%
  monitor_filter(stateCode %in% CONUS) %>%
  monitor_dailyStatistic()

allmonitors_24hr_Mtn <- monitor_loadAnnual(year = yr) %>%
  #monitor_filterDate(startdate = yyyymmdd_st, enddate = yyyymmdd_en) %>%
  monitor_filter(timezone == "America/Denver") %>%
  monitor_filter(stateCode %in% CONUS) %>%
  monitor_dailyStatistic()

allmonitors_24hr_Pacific <- monitor_loadAnnual(year = yr) %>%
  #monitor_filterDate(startdate = yyyymmdd_st, enddate = yyyymmdd_en) %>%
  monitor_filter(timezone == "America/Los_Angeles") %>%
  monitor_filter(stateCode %in% CONUS) %>%
  monitor_dailyStatistic()

allmonitors_24hr <- monitor_combine(allmonitors_24hr_Pacific,
                                    allmonitors_24hr_Mtn,
                                    allmonitors_24hr_Central,
                                    allmonitors_24hr_Eastern)
allmonitors_24hr <- monitor_dropEmpty(allmonitors_24hr)

monitor_timeseriesPlot(allmonitors_24hr,
                       addAQI = TRUE,
                       palette = "EPA",
                       NAAQS = "PM2.5_2024",
                       ylim = c(0,1200),
                       main = paste("24-hr PM2.5", yr, sep = " "))

monitor_leaflet(allmonitors_24hr, radius = 6)

len <- length(allmonitors_24hr$data)
#pm2.5 <- allmonitors_24hr$data[,2:len]

pm2.5 <- allmonitors_24hr$data[,-1]

allmonitors_24hr %>%
  monitor_timeseriesPlot(
    #shadedNight = TRUE,
    #cex = pmax(pm2.5 / 100, 0.5),
    col = aqiColors(allmonitors_24hr),
    opacity = 0.8
  )

# ----- END Susan's ------------------------------------------------------------

library(AirMonitor)

all_monitors <- monitor_loadAnnual(2024)

allmonitors_24hr_Eastern <-
  all_monitors %>%
  monitor_filter(timezone == "America/New_York") %>%
  monitor_filter(stateCode %in% CONUS) %>%
  monitor_dailyStatistic()

allmonitors_24hr_Central <-
  all_monitors %>%
  monitor_filter(timezone == "America/Chicago") %>%
  monitor_filter(stateCode %in% CONUS) %>%
  monitor_dailyStatistic()

allmonitors_24hr_Mtn <-
  all_monitors %>%
  monitor_filter(timezone == "America/Denver") %>%
  monitor_filter(stateCode %in% CONUS) %>%
  monitor_dailyStatistic()

allmonitors_24hr_Pacific <-
  all_monitors %>%
  monitor_filter(timezone == "America/Los_Angeles") %>%
  monitor_filter(stateCode %in% CONUS) %>%
  monitor_dailyStatistic()

allmonitors_24hr <-
  monitor_combine(
    allmonitors_24hr_Pacific,
    allmonitors_24hr_Mtn,
    allmonitors_24hr_Central,
    allmonitors_24hr_Eastern
  ) %>%
  monitor_dropEmpty()

### monitor_leaflet(allmonitors_24hr, radius = 6)

ids <- allmonitors_24hr$meta$deviceDeploymentID

# First blank plot to get the labeling
allmonitors_24hr %>%
  monitor_select(ids[1]) %>%
  monitor_timeseriesPlot(
    addAQI = TRUE,
    col = 'transparent',
    NAAQS = "PM2.5_2024",
    ylim = c(0,1200),
    main = "2024 24-hr PM2.5"
  )

# No 'add' each monitor plot individually
for ( i in seq_along(ids) ) {

  if ( i %% 10 == 0 )
    message(sprintf("Working on %d of %d monitors", i, length(ids)))

  id <- ids[i]

  monitor <-
    allmonitors_24hr %>%
    monitor_select(id)

  monitor %>%
    monitor_timeseriesPlot(
      col = aqiColors(monitor),
      opacity = 0.8,
      add = TRUE
    )

}


