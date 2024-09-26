<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/AirMonitor)](https://CRAN.R-project.org/package=AirMonitor)
[![Downloads](https://cranlogs.r-pkg.org/badges/AirMonitor)](https://cran.r-project.org/package=AirMonitor)
[![DOI](https://zenodo.org/badge/414793919.svg)](https://zenodo.org/badge/latestdoi/414793919)
<!-- badges: end -->

A dedicated Slack channel has been created for announcements, support and to help build a community of practice around this open source package. You may request an invitation to join from jonathan.callahan@dri.com.

# AirMonitor

```
Utilities for working with hourly air quality monitoring data
with a focus on small particulates (PM2.5). A compact data model is 
structured as a list with two dataframes. A 'meta' dataframe contains 
spatial and measuring device metadata associated with deployments at known 
locations. A 'data' dataframe contains a 'datetime' column followed by 
columns of measurements associated with each "device-deployment".
```

## Background

The USFS AirFire group is focused on air quality measurements associated with 
wildfire smoke and maintains both historical and real-time databases of PM2.5 
monitoring data obtained from stationary monitors. This data is used in 
operational displays and for retrospective analysis. Data ingest and management 
of air quality “stationary time series” are both important ongoing activities.

## Related Packages

The **[AirMonitorIngest](https://github.com/pnwairfire/AirMonitorIngest/)** 
package is used to create data archives for the **AirMonitor** package and 
isolates the work of meticulously cleaning, validating and harmonizing data from 
various sources.

The **AirMonitor** package contains data access functions to easily download 
harmonized data files as well as data manipulation functions that
make it easy to create "recipe style" analysis pipelines. The combination allows
analysts to work efficiently with short, readable R scripts. Interactive and
base R plotting functions allow for visual review of the data.

The **[AirMonitorPlots](https://github.com/mazamascience/AirMonitorPlots)**
package contains **ggplot2** based plotting functions for advanced plots.

## Installation

*NOTE:  This package has not yet been uploaded to CRAN*

Install the latest version from GitHub with:

`devtools::install_github('mazamascience/AirMonitor')`

## Data Model

The **AirMonitor** package uses the _mts_ data model defined in 
**[MazamaTimeSeries](https://mazamascience.github.io/MazamaTimeSeries/)**.

In this data model, each unique time series is referred to as a 
_"device-deployment"_ -- a timeseries collected by a particular device at a 
specific location. Multiple device-deployments are stored in memory as a
_monitor_ object -- an R list with two dataframes:

`monitor$meta` -- rows = unique device-deployments; cols = device/location metadata

`monitor$data` -- rows = UTC times; cols = device-deployments (plus an additional `datetime` column)

A key feature of this data model is the use of the `deviceDeploymentID` as a
"foreign key" that allows `data` columns to be mapped onto the associated
spatial and device metadata in a `meta` row. The following will always be true:

```
identical(names(monitor$data), c('datetime', monitor$meta$deviceDeploymentID))
```

Each column of `monitor$data` represents a timeseries associated with a particular
device-deployment while each row represents a _synoptic_ snapshot of all
measurements made at a particular time. 

In this manner, software can create both timeseries plots and maps from a single
`monitor` object in memory.

_**Note:**_ The `monitor` object time axis specified in `data$datetime` is 
guaranteed to be a regular hourly axis with no gaps.


------------------------------------------------------------------------

This project is supported by the [USFS AirFire](https://www.airfire.org) group.

