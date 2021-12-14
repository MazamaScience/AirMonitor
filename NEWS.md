# AirMonitor 0.0.8

* Added `monitor_dailyStatistic()`.
* Added `monitor_dailyThreshold()`.
* Added `monitor_dailyBarplot()`.
* Added `aqiColors()`.

# AirMonitor 0.0.7

* Added `airsis_loadLatest()`, `wrcc_loadLatest()`.
* Added `~_loadDaily()`.

# AirMonitor 0.0.6

* Now depending on **MazamaTimeSeries** 0.1.1.

# AirMonitor 0.0.5

* Guarantee that each "well known directory" path includes "/data" as the last
level before the actual data files.

# AirMonitor 0.0.4

* Added `AQSID` to `coreMetadataNames`.
* Added `monitor_toCSV()`, `monitor_toPWFSLSmoke()`.
* Added `monitor_collapse()`, `monitor_distance()` and `monitor_select()`.
* Renamed `monitor_filter()` to `monitor_filterData()` to be more explicit.
* Changed `monitor_timeseriesPlot()` default to `addAQI = FALSE`.

# AirMonitor 0.0.3

* `monitor_leaflet()` now always shows higher values on top.
* Fixed bugs in `monitor_timeseriesPlot()`.
* Added `airnow_loadlatest()`.
* Updated to expect `meta$countyName` rather than `meta$county`. This reflects
changes in **MazamaLocationutils** and thus the _mts_monitor_ objects created
by **AirMonitorIngest**.

# AirMonitor 0.0.2

* Improvements to `monitor_leaflet()`.
* Improvements to `monitor_timeseriesPlot()`.
* Added `monitor_timeInfo()` function.
* Added `CONUS` and `US_52` collections of state codes.
* Added utility function: `monitor_bestTimezone()`.
* Added support for EPA `CO`, `OZONE` and `PM10` data.
* Added `monitor_select()` for easier selection of monitors.
* Added `monitor_replaceValues()`.
* Added `QC_negativeValues` argument to `epa_aqs_loadAnnual()`

# AirMonitor 0.0.1

* Initial setup.
