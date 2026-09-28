library(AirMonitor)

ids <- c(
  "babddc7866d7c4a8_840MMFS11094",
  "0d72360824473e2a_arb3.2012",
  "121fad25495a747a_arb3.2057"
)

ca <- monitor_load(20260801, 20261001) %>% monitor_filter(stateCode == "CA")

ca %>% monitor_select(ids) %>% monitor_toCSV() %>% cat()

