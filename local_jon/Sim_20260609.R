# FASM historical is not showing SensOR/SensoWA
# FASM historicla is not showing Clarity before Dec 22, 2025

library(AirMonitor)

# ----- SensOR/WA --------------------------------------------------------------

airnow_2026 <- airnow_loadAnnual(2026)
airnow_2025 <- airnow_loadAnnual(2025)
airnow_2024 <- airnow_loadAnnual(2024)
airnow_2023 <- airnow_loadAnnual(2023)

sens_2026 <- airnow_2026 %>% monitor_filter(instrumentDescription %in% c("SensOR", "SensWA"))
sens_2025 <- airnow_2025 %>% monitor_filter(instrumentDescription %in% c("SensOR", "SensWA"))
sens_2024 <- airnow_2024 %>% monitor_filter(instrumentDescription %in% c("SensOR", "SensWA"))
sens_2023 <- airnow_2023 %>% monitor_filter(instrumentDescription %in% c("SensOR", "SensWA"))

sens <- monitor_combine(sens_2023, sens_2024, sens_2025, sens_2026)

sens %>% monitor_filterDate(20251101) %>% monitor_leaflet()

# ----- Clarity ----------------------------------------------------------------

source("~/Projects/MazamaScience/AirMonitor/local_jon/clarity_loadAnnual.R")

clarity_2026 <- clarity_loadAnnual(2026)

# > table(clarity_2026$meta$calibrationCategory)
#
# custom calibrations     global PM2.5 v1     global PM2.5 v2   global PM2.5 v2.1
#                  99                  36                  25                1128

clarity_2025 <- clarity_loadAnnual(2025)
clarity_2024 <- clarity_loadAnnual(2024)
clarity_2023 <- clarity_loadAnnual(2023)


