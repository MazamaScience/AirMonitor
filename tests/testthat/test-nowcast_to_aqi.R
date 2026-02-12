test_that("nowcast_to_aqi() matches known breakpoint values (single-column brick)", {

  # Build a single-column numeric "brick"
  brick <- function(x) {
    data.frame(d1 = x, stringsAsFactors = FALSE)
  }

  out_vals <- function(x) {
    nowcast_to_aqi(brick(x))$d1
  }

  # ---- NA / non-finite handling --------------------------------------------
  expect_true(is.na(out_vals(NA_real_)[1]))
  expect_true(is.na(out_vals(NaN)[1]))
  expect_true(is.na(out_vals(Inf)[1]))
  expect_true(is.na(out_vals(-Inf)[1]))

  # ---- zero and negative -> 0 ----------------------------------------------
  expect_equal(out_vals(0.0)[1], 0L)
  expect_equal(out_vals(-5.0)[1], 0L)

  # ---- basic known values ---------------------------------------------------
  # Good: 0.0–9.0 -> AQI 0–50
  expect_equal(out_vals(5.0)[1], 28L)
  expect_equal(out_vals(9.0)[1], 50L)

  # Moderate: 9.1–35.4 -> AQI 51–100
  expect_equal(out_vals(9.1)[1], 51L)
  expect_equal(out_vals(15.0)[1], 62L)
  expect_equal(out_vals(35.4)[1], 100L)

  # USG: 35.5–55.4 -> AQI 101–150
  expect_equal(out_vals(35.5)[1], 101L)
  expect_equal(out_vals(40.0)[1], 112L)
  expect_equal(out_vals(55.4)[1], 150L)

  # Unhealthy: 55.5–125.4 -> AQI 151–200
  expect_equal(out_vals(55.5)[1], 151L)
  expect_equal(out_vals(80.0)[1], 168L)
  expect_equal(out_vals(125.4)[1], 200L)

  # Very Unhealthy: 125.5–225.4 -> AQI 201–300
  expect_equal(out_vals(125.5)[1], 201L)
  expect_equal(out_vals(200.0)[1], 275L)
  expect_equal(out_vals(225.4)[1], 300L)

  # Hazardous: 225.5–325.4 -> AQI 301–500
  expect_equal(out_vals(225.5)[1], 301L)
  expect_equal(out_vals(325.4)[1], 500L)

  # ---- upper range extrapolates above 500 (no cap) --------------------------
  expect_equal(out_vals(400.0)[1], 649L)
  expect_equal(out_vals(1000.0)[1], 1844L)

  # ---- truncation to one decimal place -------------------------------------
  expect_equal(out_vals(9.04)[1], out_vals(9.0)[1])
  expect_equal(out_vals(9.09)[1], out_vals(9.0)[1])
  expect_equal(out_vals(9.11)[1], out_vals(9.1)[1])

  # ---- exact breakpoints ----------------------------------------------------
  expect_equal(out_vals(9.0)[1], 50L)
  expect_equal(out_vals(9.1)[1], 51L)
  expect_equal(out_vals(35.4)[1], 100L)
  expect_equal(out_vals(35.5)[1], 101L)
})
