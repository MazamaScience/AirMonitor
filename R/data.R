# ----- Example datasets ------------------------------------------------------

#' @encoding UTF-8
#' @title Example EPA AQS 88101 dataset
#' @format A tibble with 16584 rows and 44 columns of data.
#' @description The \code{example_88101} dataset provides a quickly loadable
#' version of a \emph{mts_monitor} object for practicing and code examples.
#'
#' @details
#' In the summer of 2015 Washington state had several catastrophic wildfires that led
#' to many days of heavy smoke in eastern Washington, Oregon and northern Idaho.
#' The example_88101 dataset contains EPA AQS ambient monitoring data for the
#' Pacific Northwest from May 31 through November 01, 2015. All data are
#' associated with Parameter Code 88101 -- PM2.5 FRM/FEM Mass.
#'
#' This dataset was generated on 2021-10-07 by running:
#'
#' \preformatted{
#' example_88101 <-
#'   monitor <- epa_loadAnnual(
#'     year = 2015,
#'     parameterCode = 88101,
#'     baseUrl = NULL,
#'     baseDir = "~/Data/monitoring"
#'   ) %>%
#'   monitor_filterMeta(stateCode %in% c("WA", "OR", "ID")) %>%
#'   monitor_filterDate(20150601, 20151101)
#'
#' save(example_88101, file = "data/example_88101.rda")
#' }
#'
"example_88101"