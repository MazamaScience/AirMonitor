# ----- Example datasets ------------------------------------------------------

#' @encoding UTF-8
#' @title NW_Megafires example dataset
#' @format A \emph{mts_monitor} object with 1080 rows and 143 columns of data.
#' @description The \code{NW_Megafires} dataset provides a quickly loadable
#' version of a \emph{mts_monitor} object for practicing and code examples.
#'
#' @details
#' In the summer of 2015, Washington state had several catastrophic wildfires
#' that led to many days of heavy smoke in eastern Washington, Oregon and
#' northern Idaho. The NW_Megafires dataset contains monitoring data for the
#' Pacific Northwest from July 24 through September 06, 2015.
#'
#' This dataset was generated on 2022-10-28 by running:
#'
#' \preformatted{
#' library(AirMonitor)
#'
#' NW_Megafires <-
#'   monitor_loadAnnual(2015, epaPreference = "epa_aqs") %>%
#'   monitor_filterMeta(stateCode %in% c("WA", "OR", "ID")) %>%
#'   monitor_filterDate(20150724, 20150907, timezone = "America/Los_Angeles") %>%
#'   monitor_dropEmpty()
#'
#' save(NW_Megafires, file = "data/NW_Megafires.rda")
#' }
#'
"NW_Megafires"

#' @encoding UTF-8
#' @title Carmel Valley example dataset
#' @format A \emph{mts_monitor} object with 576 rows and 2 columns of data.
#' @description The \code{Carmel_Valley} dataset provides a quickly loadable
#' version of a \emph{mts_monitor} object for practicing and code examples.
#'
#' @details
#' In August of 2016, the Soberanes fire in California burned along the Big Sur
#' coast. At the time, it was the most expensive wildfire in US history. This
#' dataset contains PM2.5 monitoring data for the monitor in Carmel Valley which
#' shows heavy smoke as well as strong diurnal cycles associated with sea
#' breezes. Data are stored as a \emph{mts_monitor} object and are used in some
#' examples in the package documentation.
#'
#' This dataset was generated on 2022-10-12 by running:
#'
#' \preformatted{
#' library(AirMonitor)
#'
#' Carmel_Valley <-
#'   airnow_loadAnnual(2016) \%>\%
#'   monitor_filterMeta(deviceDeploymentID == "a9572a904a4ed46d_840060530002") \%>\%
#'   monitor_filterDate(20160722, 20160815)
#'
#' save(Carmel_Valley, file = "data/Carmel_Valley.rda")
#' }
#'
"Carmel_Valley"

#' @encoding UTF-8
#' @title Camp Fire example dataset
#' @format A \emph{mts_monitor} object with 360 rows and 134 columns of data.
#' @description The \code{Camp_Fire} dataset provides a quickly loadable
#' version of a \emph{mts_monitor} object for practicing and code examples.
#'
#' @details
#' The 2018 Camp Fire was the deadliest and most destructive wildfire in California's
#' history, and the most expensive natural disaster in the world in 2018 in
#' terms of insured losses. The fire caused at least 85 civilian fatalities and
#' injured 12 civilians and five firefighters. It covered an area of 153,336
#' acres and destroyed more than 18,000 structures, most with the first 4 hours.
#' Smoke from the fire resulted in the worst air pollution ever for the
#' San Francisco Bay Area and Sacramento Valley.
#'
#' This dataset was was generated on 2022-10-12 by running:
#'
#' \preformatted{
#' library(AirMonitor)
#'
#' Camp_Fire <-
#'   monitor_loadAnnual(2018) \%>\%
#'   monitor_filter(stateCode == 'CA') \%>\%
#'   monitor_filterDate(
#'     startdate = 20181108,
#'     enddate = 20181123,
#'     timezone = "America/Los_Angeles"
#'  ) \%>\%
#'  monitor_dropEmpty()
#'
#' save(Camp_Fire, file = "data/Camp_Fire.rda")
#' }
"Camp_Fire"
