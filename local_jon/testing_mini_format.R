library(AirMonitor)

airnow <- airnow_loadAnnual(2023)

readr::write_csv(airnow$meta, file = "meta.csv")
readr::write_csv(airnow$data, file = "data.csv")

meta <- airnow$meta
data <- airnow$data
save(meta, file = "meta.rda")
save(data, file = "data.rda")

airnow_mini <-
  airnow %>%
  monitor_mutate(round, 0)

readr::write_csv(airnow_mini$meta, file = "mini_meta.csv", na = "")
readr::write_csv(airnow_mini$data, file = "mini_data.csv", na = "")

readr::write_csv(airnow_mini$meta, file = "mini_meta.csv.gz", na = "")
readr::write_csv(airnow_mini$data, file = "mini_data.csv.gz", na = "")


