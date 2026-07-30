# How to ingest a 175MB parquet file from Levi Stanton at Clarity
#
# From Levi Stanton:
#
# Attached is our historical open data going back to when we rolled out the v2 calibration.
#
# calibrationId CAHVV2Z9CH corresponds to v2 and CATFZKB7G4 corresponds to v2.1.
# I only included the valid data, so you won't see anything useful in the QC_FLAGS column.
# Also, our open API provides the DATASOURCE_ID, but I included the hardware's NODE_ID as well.

df <- arrow::read_parquet("~/Downloads/FASM_historical.parquet")

library(dplyr)
dplyr::glimpse(df)

df <-
  df %>%
  mutate(
    calibrationCategory = case_when(
      CALIBRATION_ID == "CAHVV2Z9CH" ~ "global_PM2.5 v2",
      CALIBRATION_ID == "CATFZKB7G4" ~ "global_PM2.5 v2.1",
      TRUE ~ NA_character_ # Handles any other IDs by assigning NA
    )
  )

df %>%
  filter(DATASOURCE_ID == "DKRPJ6886") %>%
  select(END_TIME_UTC, METRIC_VALUE) %>%
  plot(pch = 15, col = adjustcolor("black", alpha.f = 0.1))
