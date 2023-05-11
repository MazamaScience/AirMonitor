# AirMonitor 0.3.11

* Added `monitor_arrange()` to reorder time series in a _mts_monitor_ object 
based on a variable in `mts_monitor$meta`.

# AirMonitor 0.3.10

* Guarantee the existence of a `fullAQSID` field in all loaded data.

# AirMonitor 0.3.9

* Tweaks to pass CRAN checks.

# AirMonitor 0.3.8

* Fixed a bug in `monitor_loadAnnual()` when loading years before the first year
of AirNow data.
* Updated `NW_Megafires` dataset to combine data from EPA AQS, AirNow and WRCC.
* Various improvements to examples used in documentation
* Edited the old "NowCast" article for inclusion.

# AirMonitor 0.3.7

* Added `epa_aqs_loadAnnual()`.
* Updated `monitor_loadAnnual()` to load `epa_aqs` data.
* Added `epaPreference` argument to `monitor_load()`.
* Updated `monitor_combine()` with an `overlapStrategy` argument. With
`overlapStrategy = "replace all"`, values from later timeseries (including `NA`)
always replace values from earlier timeseries. With `overlapStrategy = "replace na"`, 
values from later timeseries only replace `NA` values in earlier timeseries.

# AirMonitor 0.3.6

* Updated vignettes.
* Updated `NW_Megafires` dataset from corrected database.
* Added "Save Data as CSV" article.

# AirMonitor 0.3.5

* Now depending on **MazamaTimeSeries** 0.2.8 for the `mts_selectWhere()` function.
* Updated data sets with latest version of the data.

# AirMonitor 0.3.4

* Improved logic for `QC_removeSuspectData` argument to `airsis_load~()` and
`wrcc_load~() functions.
* Added `QC_invalidateConsecutiveSuspectValues()` function.

# AirMonitor 0.3.3

* Added `QC_removeSuspectData = TRUE` argument to `airsis_load~()` and
`wrcc_load~() functions to remove those monitors that have values of 2000 ug/m3. 
A review of the data from AIRSIS and WRCC shows some archival time series 
(2015, 2016, 2020) where all values are one of 0, 1K, 2K, 3K, 4K, 5K.

# AirMonitor 0.3.2

* Added `monitor_selectWhere()` for data based selection.

# AirMonitor 0.3.1

* Updated `monitor_toPWFSLSmoke()` and `monitor_fromPWFSLSmoke()` to support
the `fullAQSID field`
* Updated `monitor_filterDate()` and `monitor_filterDatetime()` to allow one-sided
filtering when passing in only a single `startdate` or `endddate` argument.

# AirMonitor 0.3.0

Version 0.3 works with data built using a new `fullAQSID` field available from 
AirNow. This unique identifier is more consistent and should be better supported
in the future than the older `AQSID` field.

This change requires a few minor changes mostly in function examples.

# AirMonitor 0.2.2

CRAN fixes:

* Guarantee that every plotting function documents the return value.
* Updated references for AQI and NowCast algorithms.

# AirMonitor 0.2.1

CRAN fixes:

* Removed `simplfy = TRUE` from calls to `base::apply()` as this is the default.
* Replaced URL reference for AQI breaks to point to AirNow.

# AirMonitor 0.2.0

Ready for CRAN submission.

# AirMonitor 0.1.14

* Fixes for package check NOTEs.
* Added "Data Model" vignette.
* Added "Save Data as CSV" vignette.
* Changed `airnow_load~()` functions o put `parameterName` argument last.
* Added `test-loadData.R`

# AirMonitor 0.1.13

* Further documentation updates.
* Rebuilt example datasets from latest data archives.
* Removed `epa_aqs_loadAnnual()` until those data files get rebuilt.
* Improved documentation examples.
* Added `monitor_timeRange()`.
* Replace any `NaN` data values with `NA` when loading data.
* Added `AirFire_S3_archiveBaseUrl` as package data.

# AirMonitor 0.1.12

* Further documentation updates.

# AirMonitor 0.1.11

* Updated `monitor_dailyBarplot()` to use the `palette` argument.
* Updated documentation including two vignettes:
  - `Introduction to AirMonitor`
  - `Developer Style Guide`

# AirMonitor 0.1.10

* Improved error message in `monitor_combine()`.

# AirMonitor 0.1.9

* Changed default `archiveBaseUrl` to point to https://airfire-data-exports.s3.us-west-2.amazonaws.com/monitoring/v2.

# AirMonitor 0.1.8

* Added `trimEmptyDays` argument to `monitor_trimDate()`.
* Added `monitor_aqi()` and support for plots including:
  - newly defined `US_EPA$breaks_AQI`
  - added `pollutant = "AQI"` option to all `addAQI~()` functions
* Fixed bug in `monitor_leaflet()` so that it can handle single-timeseries
monitor objects.

# AirMonitor 0.1.7

* Added `monitor_load() and monitor_loadLatest/Daily/Monthly/Annual()` to 
intelligently combine data from AirNow, AIRSIS and WRCC.
* Added `airsis_loadAnnual()`.
* `monitor_leaflet()` now displays deviceDeploymentID in **bold**.

# AirMonitor 0.1.6

* Enhanced `monitor_combine()` with the `replaceMeta` argument.

# AirMonitor 0.1.5

* Added `airsis_loadAnnual()`, `wrcc_loadAnnual()`, `airnow_loadMonthly()`.
* Renamed `monitor_distance()` to `monitor_getDistance()` to imply that the 
returned object is not a _mts_monitor_ object.
* Added `monitor_filterByDistance()`.

# AirMonitor 0.1.4

* Fixed bugs in `monitor_getCurrentStatus()`:
  - error message when all yesterday data is missing in a single time zone
  - timestamps were not reported in local timezone
* Improved parameter validation in `monitor_filterDate()` and `monitor_filterDatetime()`.

# AirMonitor 0.1.3

* Added check for no-valid-values in `monitor_isEmpty()`.

# AirMonitor 0.1.2

* Renamed `monitor_extractData()` to `monitor_getData()`.
* Renamed `monitor_extractMeta()` to `monitor_getMeta()`.
* Added `monitor_getCurrentStatus()`.

# AirMonitor 0.1.1

* Added `monitor_nowcast()`.

# AirMonitor 0.1.0

* Added `monitor_dygraph()`.

# AirMonitor 0.0.9

* Added `monitor_mutate()`.
* Removed `monitor_filterData()` as too confusing because it returns an 
irregular time axis. Anyone wanting to do this kind of work should be familiar 
enough with **dplyr** to do it themselves.

# AirMonitor 0.0.8

* Added `monitor_dailyStatistic()`.
* Added `monitor_dailyThreshold()`.
* Added `monitor_dailyBarplot()`.
* Added `aqiColors()`.
* Improvements to `monitor_timeseriesPlot()` and `monitor_dailyBarplot()`.

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
