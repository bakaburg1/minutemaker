library(lubridate)

# Helper function to create a sample agenda item
create_agenda_item <- function(
  session = NULL,
  title = "Test Talk",
  from = "09:00",
  to = "10:00"
) {
  list(
    session = session,
    title = title,
    from = from,
    to = to
  )
}

# Helper function to create a sample agenda
create_sample_agenda <- function(n_items = 3) {
  purrr::map(
    seq_len(n_items),
    ~ create_agenda_item(
      session = paste0("Session", .x),
      title = paste0("Talk", .x),
      from = format(
        lubridate::ymd_hm("2024-01-01 09:00", tz = "UTC") + lubridate::hours(.x - 1),
        "%H:%M"
      ),
      to = format(
        lubridate::ymd_hm("2024-01-01 09:00", tz = "UTC") + lubridate::hours(.x),
        "%H:%M"
      )
    )
  )
}

test_that("build_ids_from_agenda handles empty agenda", {
  expect_length(build_ids_from_agenda(list()), 0)
})

test_that("build_ids_from_agenda creates correct IDs with session", {
  agenda <- create_sample_agenda(2)
  ids <- build_ids_from_agenda(agenda)
  
  expect_length(ids, 2)
  expect_identical(ids[1], "Session1_Talk1")
  expect_identical(ids[2], "Session2_Talk2")
})

test_that("build_ids_from_agenda handles NULL titles", {
  agenda <- list(
    list(session = "Session1", title = NULL),
    list(session = "Session2", title = NULL)
  )
  ids <- build_ids_from_agenda(agenda)
  
  expect_length(ids, 2)
  expect_identical(ids[1], "Session1_1")
  expect_identical(ids[2], "Session2_2")
})

test_that("build_ids_from_agenda handles NULL sessions", {
  agenda <- list(
    list(session = NULL, title = "Talk1"),
    list(session = NULL, title = "Talk2")
  )
  ids <- build_ids_from_agenda(agenda)
  
  expect_length(ids, 2)
  expect_identical(ids[1], "Talk1")
  expect_identical(ids[2], "Talk2")
})

test_that("convert_agenda_times validates input times with specific warnings and error", {
  # Invalid 'from' time
  invalid_from_item <- create_agenda_item(from = "invalid")
  expect_snapshot(error = TRUE, {
    convert_agenda_times(
      invalid_from_item,
      convert_to = "seconds",
      event_start_time = "09:00"
    )
  })

  # Invalid 'to' time
  invalid_to_item <- create_agenda_item(to = "invalid")
  expect_snapshot(error = TRUE, {
    convert_agenda_times(
      invalid_to_item,
      convert_to = "seconds",
      event_start_time = "09:00"
    )
  })
})

test_that("convert_agenda_times handles single agenda item", {
  item <- create_agenda_item(from = "09:00", to = "10:00")
  # This case should not produce warnings or errors with valid input
  expect_no_warning(converted <- convert_agenda_times(
    item,
    convert_to = "seconds",
    event_start_time = "09:00"
  ))
  expect_no_error(converted) # Redundant if expect_no_warning passed and assigned
  
  expect_type(converted$from, "double")
  expect_type(converted$to, "double")
  expect_equal(converted$from, 0)
  expect_equal(converted$to, 3600)
})

test_that("convert_agenda_times converts clock time to seconds", {
  agenda <- create_sample_agenda(2)
  # Should not produce warnings with valid inputs
  expect_no_warning(converted <- convert_agenda_times(
    agenda,
    convert_to = "seconds",
    event_start_time = "09:00"
  ))
  expect_no_error(converted)
  
  expect_type(converted[[1]]$from, "double")
  expect_type(converted[[1]]$to, "double")
  expect_equal(converted[[1]]$from, 0)
  expect_equal(converted[[1]]$to, 3600)
  expect_equal(converted[[2]]$from, 3600)
  expect_equal(converted[[2]]$to, 7200)
})

test_that("convert_agenda_times converts seconds to clock time", {
  agenda <- list(
    list(from = 0, to = 3600),
    list(from = 3600, to = 7200)
  )
  expect_no_warning(converted <- convert_agenda_times(
    agenda,
    convert_to = "clocktime",
    event_start_time = "09:00"
  ))
  expect_no_error(converted)
  
  expect_type(converted[[1]]$from, "character")
  expect_type(converted[[1]]$to, "character")
  expect_equal(converted[[1]]$from, "09:00:00")
  expect_equal(converted[[1]]$to, "10:00:00")
  expect_equal(converted[[2]]$from, "10:00:00")
  expect_equal(converted[[2]]$to, "11:00:00")
})

test_that("convert_agenda_times handles custom event start time", {
  agenda <- create_sample_agenda(1)
  agenda[[1]]$from <- "15:00"
  agenda[[1]]$to <- "16:00"
  
  expect_no_warning(converted <- convert_agenda_times(
    agenda,
    convert_to = "seconds",
    event_start_time = "14:00"
  ))
  expect_no_error(converted)
  
  expect_equal(converted[[1]]$from, 3600)
  expect_equal(converted[[1]]$to, 7200)
})

test_that("convert_agenda_times handles POSIXct event start time", {
  start_time <- lubridate::ymd_hms("2024-01-01 14:00:00", tz = "UTC")
  
  agenda <- list(
    list(
      from = lubridate::ymd_hms("2024-01-01 15:00:00", tz = "UTC"),
      to = lubridate::ymd_hms("2024-01-01 16:00:00", tz = "UTC")
    )
  )
  
  stopifnot(!is.na(start_time))
  stopifnot(!is.na(agenda[[1]]$from))
  stopifnot(!is.na(agenda[[1]]$to))
  
  expect_no_warning(converted <- convert_agenda_times(
    agenda,
    convert_to = "seconds",
    event_start_time = start_time
  ))
  expect_no_error(converted)
  
  expect_equal(converted[[1]]$from, 3600)
  expect_equal(converted[[1]]$to, 7200)
})

test_that("convert_agenda_times warns when no event start time provided", {
  agenda <- create_sample_agenda(1)
  expect_warning(
    convert_agenda_times(agenda, convert_to = "seconds"),
    "No start time provided"
  )
})

test_that("convert_agenda_times handles mixed time formats with specific warnings and error", {
  mixed_agenda <- list(
    list(from = "09:00", to = 3600), # item 1: char, num
    list(from = 3600, to = "11:00")  # item 2: num, char
  )
  
  expect_snapshot(error = TRUE, {
    convert_agenda_times(mixed_agenda)
  })
})

test_that("convert_agenda_times handles invalid event start time format", {
  agenda <- create_sample_agenda(1)
  expect_error(
    convert_agenda_times(agenda, event_start_time = 123),
    "Invalid event start time format"
  )
})

test_that("convert_agenda_times respects custom conversion format", {
  agenda <- list(list(from = 0, to = 3600))
  expect_no_warning(converted <- convert_agenda_times(
    agenda,
    convert_to = "clocktime",
    event_start_time = "09:00",
    conversion_format = "%R"
  ))
  expect_no_error(converted)
  
  expect_equal(converted[[1]]$from, "09:00")
  expect_equal(converted[[1]]$to, "10:00")
})

cat("\nAll testthat tests for agenda_management.R defined.\n") 