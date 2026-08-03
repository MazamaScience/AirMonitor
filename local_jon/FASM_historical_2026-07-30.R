# Email forwarded from Laura.L.Warren@mass.gov on 7/20/2026

# I’d like to report a couple of bugs on FASM Historical. Massachusetts recently
# had some PM2.5 exceedances due to smoke impacts and when checking some of these
# days in FASM Historical, some of the exceedances are being displayed as having
# Daily Avg AQI in the upper Moderate range instead of reaching Unhealthy for
# Sensitive Groups. A list of exceeding daily average PM2.5 concentrations are
# in the table below from the MassDEP Air Assessment Branch (subject to further
# QA/QC). Using these concentrations, I’ve calculated their AQI values and added
# them to the table. Some example FASM Historical screenshots are also attached.
#
# Bugs are described below:
#   On 7/16/26, when selecting the Pittsfield permanent monitor and Daily + Avg
# options, it shows a “NowCast AQI” of 100. It should be showing a daily AQI = 106.
# On 7/18/26, using the same options but different date, one MA permanent monitor
# was showing Moderate instead of USG – Weymouth. For context, on 7/18, all of
# our Massachusetts permanent monitors exceeded the daily PM2.5 NAAQS except for
# 3 monitors (Pittsfield, North Adams, and Fall River). The Weymouth monitor
# (south of Boston) shows up as upper Moderate AQI on FASM Historical and yet it
# should show daily avg AQI = 102.
#
# For both days selected, I chose the correct date in the slider, but the pop-up
# that appears when hovering over the AQI bar chart cites the day before my chosen
# date. I chose 7/16 for example, then it highlights the bar chart and the pop-up says 7/15.
#
# Question - is FASM Historical basing its daily average calculation on NowCast concentrations?
#   For a true daily average AQI, hourly PM2.5 concentrations should be used,
#   instead of NowCast. Otherwise, it will conflict with the daily AQI data that
#   are provided on other AirNow.gov pages (e.g., AirNow Interactive Map Archive for 7/16)

library(AirMonitor)

airnow_2026 <- airnow_loadAnnual(2026)

mass <-
  airnow_2026 %>%
  monitor_filter(stateCode == "MA") %>%
  monitor_filterDate("2026-07-13", "2026-07-20")

###mass %>% monitor_leaflet()

Pittsfield <-
  mass %>%
  monitor_select("drect2h_840250030008")

################################################################################
# Hourly time series

layout(seq(2))

Pittsfield %>%
  monitor_timeseriesPlot(
    shadedNight = TRUE,
    addAQI = TRUE
  )

Pittsfield %>%
  monitor_aqi(includeShortTerm = TRUE) %>%
  monitor_timeseriesPlot(
    shadedNight = TRUE,
    addAQI = TRUE
  )

layout(1)

################################################################################
# Daily averages

# ----- Daily PM2.5 ------------------------------------------------------------

daily_avg <-
  Pittsfield %>%
  monitor_dailyStatistic(FUN = mean, dayBoundary = "LST") %>%
  monitor_getData() %>%
  dplyr::mutate(datetime = as.Date(datetime)) %>%
  dplyr::rename_with(~ c("date", "daily avg PM2.5"))

# ----- Daily AQI --------------------------------------------------------------

# 1) Calculate daily average
monitor <-
  Pittsfield %>%
  monitor_dailyStatistic(FUN = mean, dayBoundary = "LST")

# 2) Calculate AQI
dataBrick <- dplyr::select(monitor$data, -1)

digits <- 1

dataBrick <- trunc(dataBrick*10^digits)/10^digits

# Function name is inaccurate. Should be 'data_to_aqi'.
monitor$data[,-1] <- nowcast_to_aqi(dataBrick)

daily_aqi <-
  monitor %>%
  monitor_getData() %>%
  dplyr::mutate(datetime = as.Date(datetime)) %>%
  dplyr::rename_with(~ c("date", "daily AQI"))


# Combine and print
daily_avg %>%
  dplyr::left_join(daily_aqi) %>%
  print()


# YAY! ==> This matches AirNow:
#
# https://gispub.epa.gov/airnow/?forecastcontours=forecasttoday&tab=archive&archivedates=07%2F16%2F2026&contours=none&monitors=pm25&showgreencontours=false&xmin=-8716911.389213374&xmax=-7256658.400853755&ymin=4633185.754410612&ymax=5656830.43720542
#

