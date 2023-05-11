# Create a daily average time series plot and a map for 2022

# ----- Load 2022 monitor data -------------------------------------------------

library(AirMonitor)

all_monitors <-

  # 1) load all monitors
  #    - negative values are lifted up to zero
  #    - use AirNow data rather than EPA AQS
  monitor_loadAnnual(
    year = 2022,
    QC_negativeValues = "zero",
    epaPreference = "airnow"
  ) %>%

  # 2) US only
  monitor_filter(countryCode == "US") %>%

  # 3) drop any monitors with no data
  monitor_dropEmpty()

# ----- Calculate daily means --------------------------------------------------

# NOTE:  Calculating daily means will depend upon the timezone so, from here on,
# NOTE:  we do things on a per-timezone basis.

dailyMeanList <- list()

for ( tz in unique(all_monitors$meta$timezone) ) {

  dailyMeanList[[tz]] <-

    all_monitors %>%
    monitor_filter(timezone == tz) %>%
    monitor_filterDate(20220415, 20221101) %>%
    monitor_dailyStatistic(
      FUN = mean,
      na.rm = TRUE,
      minHours = 18,
      dayBoundary = "LST"
    )

}

# ----- Timeseries plot --------------------------------------------------------

# Initialize the plot axes by plotting with 'transparent' color
monitor_timeseriesPlot(
  dailyMeanList[[1]],
  ylab = "24-hr Average PM2.5 (micrograms/m\u00b3)",
  xlab = "2022",
  ylim = c(0, 800),
  col = 'transparent',
  main = '',
  addAQI = TRUE
)

# Then add each timezone in succession
for ( tz in unique(all_monitors$meta$timezone) ) {

  print(tz)
  monitor_timeseriesPlot(
    dailyMeanList[[tz]],
    opacity = 0.3,
    main = '',
    add = TRUE
  )

}

addAQILegend("topright", title = "Daily Average AQI")

title("2022 Timing of Smoke Impacts over entire US (all AQI levels), Apr 15-Nov 1")

# ----- Leaflet map ------------------------------------------------------------

library(leaflet)

airnow_meta <-
  all_monitors %>%
  monitor_filter(dataIngestSource == "AIRNOW") %>%
  monitor_getMeta()

airsis_meta <-
  all_monitors %>%
  monitor_filter(dataIngestSource == "AIRSIS") %>%
  monitor_getMeta()

wrcc_meta <-
  all_monitors %>%
  monitor_filter(dataIngestSource == "WRCC") %>%
  monitor_getMeta()

map <-
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircles(data = airnow_meta, color = "red") %>%
  addCircles(data = airsis_meta, color = "blue") %>%
  addCircles(data = wrcc_meta, color = "green") %>%

print(map)

