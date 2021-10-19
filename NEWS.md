# AirMonitor 0.0.3

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
