# Tests for is_silent() ---

test_that("is_silent correctly identifies silent segments", {
  expect_true(is_silent("[...]"))
  expect_true(is_silent(NA_character_))
  expect_true(is_silent(""))
  expect_false(is_silent("some text"))
  expect_false(is_silent(" NA "))
  expect_identical(
    is_silent(c("[...]", "text", NA, "")),
    c(TRUE, FALSE, TRUE, TRUE)
  )
})

test_that("silent returns the correct silent indicator", {
  expect_identical(silent(), "[...]")
})

# Tests for parse_event_time() ---

test_that("parse_event_time handles POSIXct input", {
  now <- Sys.time()
  expect_identical(parse_event_time(now), now)
})

test_that("parse_event_time parses valid time strings", {
  expect_s3_class(parse_event_time("14:30"), "POSIXct")
  expect_s3_class(parse_event_time("14:30:45"), "POSIXct")
  expect_equal(
    as.character(lubridate::floor_date(parse_event_time("14:30"), "seconds")),
    as.character(lubridate::parse_date_time("14:30", orders = c("HM", "HMS")))
  )
  expect_equal(
    as.character(lubridate::floor_date(
      parse_event_time("14:30:15"),
      "seconds"
    )),
    as.character(lubridate::parse_date_time(
      "14:30:15",
      orders = c("HM", "HMS")
    ))
  )
})

test_that("parse_event_time handles unparseable time strings gracefully", {
  expect_warning(time_na <- parse_event_time("invalid_time"))
  expect_true(is.na(time_na))

  expect_warning(time_na_format <- parse_event_time("25:00", format = "HM"))
  expect_true(is.na(time_na_format))
})

test_that("parse_event_time throws error for invalid input types", {
  expect_error(parse_event_time(123), regexp = "Invalid time format provided")
  expect_error(
    parse_event_time(list(a = 1)),
    regexp = "Invalid time format provided"
  )
  expect_error(parse_event_time(TRUE), regexp = "Invalid time format provided")
})

# Tests for time_to_numeric() ---

test_that("time_to_numeric handles valid numeric input", {
  expect_equal(time_to_numeric(3600), 3600)
  expect_equal(time_to_numeric(0), 0)
})

test_that("time_to_numeric converts valid time strings to numeric", {
  expect_equal(time_to_numeric("01:00:00"), 3600)
  expect_equal(time_to_numeric("00:00:00"), 0)
  expect_equal(time_to_numeric("10:00"), 10 * 3600)
})

test_that("time_to_numeric handles POSIXct inputs", {
  time_posixct <- lubridate::parse_date_time("01:00:00", orders = "HMS")
  origin_posixct <- lubridate::parse_date_time("00:00:00", orders = "HMS")
  expect_equal(
    time_to_numeric(time_posixct),
    as.numeric(time_posixct) - as.numeric(origin_posixct)
  )
})

test_that("time_to_numeric calculates difference from origin", {
  expect_equal(time_to_numeric("01:00:00", origin = "00:30:00"), 1800)
  expect_equal(time_to_numeric(3600, origin = 1800), 1800)

  time_posixct <- lubridate::parse_date_time("02:00:00", orders = "HMS")
  origin_posixct <- lubridate::parse_date_time("01:00:00", orders = "HMS")
  expect_equal(time_to_numeric(time_posixct, origin = origin_posixct), 3600)

  expect_equal(time_to_numeric(time_posixct, origin = "01:00:00"), 3600)
  expect_equal(time_to_numeric("02:00:00", origin = origin_posixct), 3600)
})

test_that("time_to_numeric throws error for invalid input types for time", {
  expect_error(time_to_numeric(TRUE), "Invalid format for .*time.* argument")
  expect_error(time_to_numeric(list()), "Invalid format for .*time.* argument")
})

test_that("time_to_numeric throws error for invalid input types for origin", {
  expect_error(
    time_to_numeric("01:00:00", origin = TRUE),
    "Invalid format for .*origin.* argument"
  )
  expect_error(
    time_to_numeric("01:00:00", origin = list()),
    "Invalid format for .*origin.* argument"
  )
})

test_that("time_to_numeric throws error for incompatible time and origin types", {
  expect_error(
    time_to_numeric(3600, origin = "00:00:00"),
    ".*time.* and .*origin.* arguments have incompatible types"
  )
  expect_error(
    time_to_numeric("01:00:00", origin = 0),
    ".*time.* and .*origin.* arguments have incompatible types"
  )
})

test_that("time_to_numeric throws error for negative time difference", {
  expect_error(
    time_to_numeric("01:00:00", origin = "02:00:00"),
    "Calculated time difference is negative"
  )
  expect_error(
    time_to_numeric(0, origin = 3600),
    "Calculated time difference is negative"
  )

  time_posixct_earlier <- lubridate::parse_date_time("01:00:00", orders = "HMS")
  origin_posixct_later <- lubridate::parse_date_time("02:00:00", orders = "HMS")
  expect_error(
    time_to_numeric(time_posixct_earlier, origin = origin_posixct_later),
    "Calculated time difference is negative"
  )
})

test_that("time_to_numeric handles unparseable time strings via parse_event_time", {
  expect_warning(
    res1 <- time_to_numeric("invalid", origin = "00:00:00"),
    regexp = "All formats failed to parse. No formats found."
  )
  expect_true(is.na(res1))
  expect_warning(
    res2 <- time_to_numeric("01:00:00", origin = "invalid"),
    regexp = "All formats failed to parse. No formats found."
  )
  expect_true(is.na(res2))

  {
    res3 <- time_to_numeric("invalid1", origin = "invalid2")
  } |>
    expect_warning("All formats failed to parse. No formats found.") |>
    expect_warning("All formats failed to parse. No formats found.")
  expect_true(is.na(res3))

  expect_warning(
    res4 <- time_to_numeric("invalid"),
    regexp = "All formats failed to parse. No formats found."
  )
  expect_true(is.na(res4))
})

