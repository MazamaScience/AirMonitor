test_that("monitor_aqi() succeeds on Carmel_Valley example data", {

  data("Carmel_Valley", package = "AirMonitor", envir = environment())

  # Expect success: i.e., no error should be thrown.
  expect_error(
    monitor_aqi(Carmel_Valley),
    NA
  )

})
